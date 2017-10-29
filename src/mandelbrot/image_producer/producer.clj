(ns mandelbrot.image-producer.producer
  (:require [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.locations :as l]
            [mandelbrot.coloring :as co]

            [clojure.core.async :refer [<!!]]
            [helpers.general-helpers :as g])

  (:import [java.awt.image BufferedImage]
           [java.awt Color Graphics2D RenderingHints]
           [java.io File]
           [javax.imageio ImageIO]
           (java.util Date)))

(def save-path "./saves/")

(def color-mode BufferedImage/TYPE_INT_RGB)
(def save-ext "png")

(defn new-image-for-limits ^BufferedImage [limits]
  (let [{:keys [rep-width rep-height]} limits
        ^BufferedImage img (BufferedImage. rep-width rep-height color-mode)]

    img))

(defn draw-image-in-limits [limits in-chan color-f chunk-f]
  (let [img (new-image-for-limits limits)
        {:keys [rep-width rep-height]} limits]

    (loop [chunk-n 0]
      (when-let [chunk (<!! in-chan)]
        (chunk-f chunk-n chunk)

        (doseq [{:keys [rep-x rep-y iters]} chunk]
          (let [^Color color (color-f rep-x rep-y iters)]
            (.setRGB img rep-x rep-y
                     (.getRGB color))))

        (recur (inc chunk-n))))

    img))

(defn draw-image-in-limits2 [limits points color-f chunk-f]
  (let [{:keys [rep-width rep-height]} limits
        img (new-image-for-limits limits)]

    (loop [point-n 0
           [{:keys [r i rep-x rep-y iters] :as point} & rest-points] points]
      (chunk-f point-n point)

      (if point
        (let [^Color color (color-f r i iters)]
          (.setRGB img rep-x rep-y
                   (.getRGB color))

          (recur (inc point-n) rest-points))

        img))))

(defn limits->std-name [limits]
  (let [{:keys [start-r end-r, start-i end-i]} limits]
    (str "([" start-r "," end-r "] - [" start-i "," end-i "])")))

(defn save-image [limits, ^BufferedImage img]
  (let [file-name (str (limits->std-name limits) "-" (g/current-ms-timestamp))]
    (ImageIO/write img
                   ^String save-ext
                   (File. (str save-path file-name "." save-ext)))))

(defn perc-done [current-n limits]
  (let [{:keys [rep-width rep-height]} limits]
    (double (/ current-n (* rep-width rep-height)))))

(defn test-routine []
  (let [r-width 5472
        r-height 3648
        total (* r-width r-height)
        l (assoc l/swirl :rep-width r-width, :rep-height r-height)
        perc #(perc-done % l)
        print-perc 0.05
        print-every (int (* total print-perc))

        points (cf/lazy-par-calc-points l)

        img (time
              (draw-image-in-limits2 l points co/exp
                (fn [n c]
                  ; TODO: Update a progress bar
                  (when (zero? (rem n print-every))
                    (println "N:" n "-"
                             (str (format "%.1f" (* (perc n) 100)) "%"))))))]

    (println "Image drawn...")

    (save-image l img)
    (println "Saved!")

    (println "Stopped.")))

