(ns mandelbrot.seesaw.helpers
  (:require [seesaw.core :as sc])
  (:import (java.awt Dimension)))

(defn get-size [component]
  (let [^Dimension dims (sc/config component :size)]
    [(.width dims) (.height dims)]))

(defn when-done [future callback]
  (future (callback @future)))

(defn move-limits-to [enclosing-limits center-x center-y]
  (let [{:keys [start-r end-r, start-i end-i]} enclosing-limits
        plane-dims [(- end-r start-r) (- end-i start-i)]
        [h-width h-height] (mapv #(/ % 2) plane-dims)]

    (assoc enclosing-limits
      :start-r (- center-x h-width)
      :end-r (+ center-x h-width)
      :start-i (- center-y h-height)
      :end-i (+ center-y h-height))))

(defn zoom-limits
  "Zooms a limit view in or out, depending on the in? parameter."
  [limits in? zoom-by]
  (let [{:keys [start-r end-r, start-i end-i]} limits
        zoom-dir (if in? 1 -1)
        half-z (/ zoom-by 2)
        diff (* half-z zoom-dir)]

    (assoc limits
      :start-r (+ start-r diff)
      :end-r (- end-r diff)
      :start-i (+ start-i diff)
      :end-i (- end-i diff))))

(defn zoom-limits-by-perc
  "Zooms a limit view in or out, depending on the in? parameter."
  [limits in? zoom-perc]
  (let [{:keys [start-r end-r, start-i end-i]} limits

        view-dims [(- end-r start-r) (- end-i start-i)]
        half-dims (mapv #(/ % 2) view-dims)

        zoom-by (* zoom-perc (apply min half-dims))]

    (println "Zoom by" zoom-by)

    (zoom-limits limits in? zoom-by)))




