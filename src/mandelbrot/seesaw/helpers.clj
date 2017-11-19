(ns mandelbrot.seesaw.helpers
  (:require [seesaw.core :as sc])
  (:import (java.awt Dimension Image Component)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.util Date)
           (java.io File)))

(defn update-config! [^Component c, accessor-key, f]
  (sc/config! c accessor-key
    (f (sc/config c accessor-key))))

(defn get-dimensions [component]
  (let [^Dimension dims (sc/config component :size)]
    [(.width dims) (.height dims)]))

(defn plane-dimensions [limits]
  (let [{:keys [start-r end-r, start-i end-i]} limits]

    [(- end-r start-r)
     (- end-i start-i)]))

(defn rep-ratio [limits]
  (let [{:keys [rep-width rep-height]} limits]
    (/ rep-height rep-width)))

(defn fix-plane-height
  "Adjusts the limits of the complex plane to match the height/width ratio of the representation."
  [limits]
  (let [rat (rep-ratio limits)
        {:keys [start-r, start-i]} limits
        [w h] (plane-dimensions limits)]

    (assoc limits
      :end-i (double (+ start-i (* rat w))))))

(defn move-limits-to [limits center-r center-i]
  (let [plane-dims (plane-dimensions limits)
        [h-width h-height] (mapv #(/ % 2) plane-dims)]

    (assoc limits
      :start-r (- center-r h-width)
      :end-r (+ center-r h-width)
      :start-i (- center-i h-height)
      :end-i (+ center-i h-height))))

(defn move-limits-by [limits r-offset i-offset]
  (let [{:keys [start-r start-i]} limits
        [width height] (plane-dimensions limits)
        center-r (double (+ start-r (/ width 2) r-offset))
        center-i (double (+ start-i (/ height 2) i-offset))]

    (move-limits-to limits center-r center-i)))

(defn zoom-limits
  "Zooms a limit view in or out, depending on the in? parameter."
  [limits in? zoom-by]
  (let [{:keys [start-r end-r, start-i end-i]} limits
        zoom-dir (if in? 1 -1)
        diff (* zoom-by zoom-dir)]

    (assoc limits
      :start-r (+ start-r diff)
      :end-r (- end-r diff)
      :start-i (+ start-i diff)
      :end-i (- end-i diff))))

(defn zoom-limits-by-perc
  "Zooms a limit view in or out, depending on the in? parameter."
  [limits in? zoom-perc]
  (let [view-dims (plane-dimensions limits)
        half-dims (mapv #(/ % 2) view-dims)

        zoom-by (* zoom-perc (apply min half-dims))]

    (zoom-limits limits in? zoom-by)))

(defn available-processors []
  (doto (.availableProcessors (Runtime/getRuntime))
        (println "available processors")))

(defn get-snapshot [^Component canvas]
  (let [^Image img (BufferedImage. (.getWidth canvas) (.getHeight canvas)
                                   BufferedImage/TYPE_INT_RGB)]

    (.paint canvas (.getGraphics img))

    img))

(defn save-snapshot [limits, ^BufferedImage img]
  (let [ext "jpg"
        file-name "save" #_(str (into {} limits))]
    (ImageIO/write img ext (File. (str file-name "." ext)))))



