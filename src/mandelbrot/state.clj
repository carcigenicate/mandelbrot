(ns mandelbrot.state
  (:require [quil.core :as q]
            [helpers.general-helpers :as g])
  (:import (java.math RoundingMode MathContext)))

(set! *math-context* (MathContext. 100 RoundingMode/HALF_UP))

(defrecord Mandel-Limits [x-min x-max y-min y-max]
  Object
  (toString [self] (str "x[" x-min "," x-max "] y[" y-min "," y-max "]")))

(defrecord State [mandel-limits rows])

(def initial-limits (->Mandel-Limits (bigdec -2) (bigdec 2)
                                     (bigdec -2) (bigdec 2)))

(defn initial-state [mandel-points]
  (->State initial-limits mandel-points))

(defn pixel-row-partitions [width height]
  (for [x (range width)]
    (for [y (range height)]
      [x y])))

(defn map-dimension [n screen-dim-max dimension-min dimension-max]
  (g/map-range n 0 screen-dim-max dimension-min dimension-max))

(defn screen-coord-to-mandel-point [x y screen-width screen-height limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(map-dimension x screen-width x-min x-max)
     (map-dimension y screen-height y-min y-max)]))

(defn mandel-point-to-screen-point [a b screen-width screen-height limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(Math/round ^float (g/map-range a x-min x-max 0 screen-width))
     (Math/round ^float (g/map-range b y-min y-max 0 screen-height))]))

(defn screen-points-to-mandel [screen-points screen-width screen-height limits]
  (map (fn [[x y]] (screen-coord-to-mandel-point x y screen-width screen-height limits))
       screen-points))

(defn mandel-row-partitions [limits screen-width screen-height]
  (let [rows (pixel-row-partitions screen-width screen-height)]
    (map #(screen-points-to-mandel % screen-width screen-height limits) rows)))

(defn populate-rows [state screen-width screen-height]
  (assoc state :rows
               (mandel-row-partitions (:mandel-limits state)
                                      screen-width screen-height)))

(defn range-test []
  (let [nmr q/map-range
        mmr g/map-range
        v 5
        s1 0 e1 10
        s2 10 e2 20]
    (println (nmr v s1 e1 s2 e2)
             (mmr v s1 e1 s2 e2))))


