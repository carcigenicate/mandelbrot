(ns mandelbrot.viewport-state
  (:require [quil.core :as q]
            [helpers.general-helpers :as g]))

(def ^:const mapping-precision 10) ; TODO: Make ~100?

(set! *warn-on-reflection* true)

(defrecord Limits [x-min x-max y-min y-max]
  Object
  (toString [self] (str "x[" x-min "," x-max "] y[" y-min "," y-max "]")))

(defrecord Viewport-State [mandel-limits screen-limits])

(defn new-zero-based-limits [max-x max-y]
  (->Limits 0M max-x 0M max-y))

; TODO: Eww
(defn screen-to-mandel [x y view-state]
  (let [{{mx-mi :x-min, mx-ma :x-max,
          my-mi :y-min, my-ma :y-max} :mandel-limits
         {sx-mi :x-min, sx-ma :x-max,
          sy-mi :y-min, sy-ma :y-max} :screen-limits} view-state]

    (with-precision mapping-precision
      [(g/map-range x sx-mi sx-ma mx-mi mx-ma)
       (g/map-range y sy-mi sy-ma my-mi my-ma)])))

(defn mandel-to-screen [a b view-state]
  (let [{{mx-mi :x-min, mx-ma :x-max,
          my-mi :y-min, my-ma :y-max} :mandel-limits
         {sx-mi :x-min, sx-ma :x-max,
          sy-mi :y-min, sy-ma :y-max} :screen-limits} view-state]

    [(g/map-range a mx-mi mx-ma sx-mi sx-ma)
     (g/map-range b my-mi my-ma sy-mi sy-ma)]))


(defn limit-dimension-sizes [limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(- x-max x-min)
     (- y-max y-min)]))