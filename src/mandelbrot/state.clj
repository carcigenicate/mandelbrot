(ns mandelbrot.state
  (:require [quil.core :as q]
            [helpers.general-helpers :as g])
  (:import (java.math RoundingMode MathContext)))

(set! *warn-on-reflection* true)

(defrecord Mandel-Limits [x-min x-max y-min y-max]
  Object
  (toString [self] (str "x[" x-min "," x-max "] y[" y-min "," y-max "]")))

(defrecord State [mandel-limits rows zoom-factor])

(def limit-type-caster float)

; Change back to bigdec at some point
(def initial-limits (->Mandel-Limits (limit-type-caster -2) (limit-type-caster 2)
                                     (limit-type-caster -2) (limit-type-caster 2)))

(defn initial-state [mandel-points]
  (->State initial-limits mandel-points 1))

(defn pixel-row-partitions [width height]
  (for [x (range width)]
    (for [y (range height)]
      [x y])))

(defn map-range [value start1 stop1 start2 stop2]
  (q/map-range value start1 stop1 start2 stop2))

(defn map-dimension [n screen-dim-max dimension-min dimension-max]
  (map-range n 0 screen-dim-max dimension-min dimension-max))

(defn screen-coord-to-mandel-point [x y screen-width screen-height limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(map-dimension x screen-width x-min x-max)
     (map-dimension y screen-height y-min y-max)]))

(defn mandel-point-to-screen-point [a b screen-width screen-height limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(Math/round ^float (map-range a x-min x-max 0 screen-width))
     (Math/round ^float (map-range b y-min y-max 0 screen-height))]))

(defn screen-points-to-mandel [screen-points screen-width screen-height limits]
  ; Use pmap?
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

(defn time-test []
  (let [f long
        v (f 5)
        s1 (f 0) e1 (f 10)
        s2 (f 10) e2 (f 20)]
    (g/bench
      (g/map-range v s1 e1 s2 e2))))


