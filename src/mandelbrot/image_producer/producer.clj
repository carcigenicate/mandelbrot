(ns mandelbrot.image-producer.producer
  (:require [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.locations :as l]
            [mandelbrot.coloring :as co]

            [clojure.core.async :refer [<!!]]
            [helpers.general-helpers :as g]
            [seesaw.core :as sc])

  (:import [java.awt.image BufferedImage]
           [java.awt Color Graphics2D RenderingHints]
           [java.io File]
           [javax.imageio ImageIO]
           [java.util Date]))

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
          (let [^Color color (color-f rep-x rep-y iters)
                rgb (.getRGB color)]
            (.setRGB img rep-x rep-y rgb)))

        (recur (inc chunk-n))))

    img))

; Accepts a (potentially lazy) list of points to process, instead of a channel.
(defn draw-image-in-limits2 [limits points color-f point-f]
  (let [{:keys [rep-width rep-height]} limits
        img (new-image-for-limits limits)]

    (loop [point-n 0
           [{:keys [r i rep-x rep-y iters] :as point} & rest-points] points]
      (point-f point-n point)

      (if point
        (let [^Color color (color-f r i iters)
              rgb (.getRGB color)]
          (.setRGB img rep-x rep-y rgb)

          (recur (inc point-n) rest-points))

        img))))

(defn limits->std-name [limits]
  (let [{:keys [start-r end-r, start-i end-i]} limits
        f #(format "%.16f" (double %))]

    (str "["(f start-r) " " (f end-r) " " (f start-i) " " (f end-i) "]")))

(defn save-image [limits, ^BufferedImage img]
  (let [file-name (str (limits->std-name limits) " Date " (g/current-ms-timestamp))
        path (str save-path file-name "." save-ext)]
    (clojure.java.io/make-parents path)

    (ImageIO/write img, ^String save-ext, (File. path))))

(defn perc-done [current-n limits]
  (let [{:keys [rep-width rep-height]} limits]
    (double (/ current-n (* rep-width rep-height)))))

(defn minutes-remaining [jobs-completed total-jobs ms-elapsed]
  (if (zero? jobs-completed)
    0.0

    (let [ms-per (/ ms-elapsed jobs-completed)
          remaining-jobs (- total-jobs jobs-completed)
          ms-remaining (* remaining-jobs ms-per)]

      (double (/ ms-remaining 1000 60)))))

(defn formatted-mins-left [jobs-completed total-jobs ms-elapsed]
  (str (format "%.2f" (minutes-remaining jobs-completed total-jobs ms-elapsed))
       " mins"))

(defn canvas-saver [prog-bar time-label color-f save-limits]
  (let [perc #(perc-done % save-limits)
        {:keys [rep-width rep-height]} save-limits

        total (* rep-width rep-height)
        update-perc 0.01
        update-every (int (* total update-perc))

        points (cf/lazy-par-calc-points save-limits)

        t #(g/current-ms-timestamp)
        start-ms (t)

        img (draw-image-in-limits2 save-limits points color-f
              (fn [n c]
               (when (zero? (rem n update-every))
                 (sc/invoke-later
                   (sc/value! prog-bar (* 100 (perc n)))
                   (sc/text! time-label
                             (formatted-mins-left n total (- (t) start-ms)))))))]

    (save-image save-limits img)))

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
    (println "Saved!")))