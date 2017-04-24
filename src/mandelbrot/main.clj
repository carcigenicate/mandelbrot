(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]

            [helpers.general-helpers :as g])

  (:gen-class))

(def screen-width 200)
(def screen-height 200)

(def max-tests 200)

(defn all-pixels-to-draw []
  (into #{}
        (for [y (range screen-height)
              x (range screen-width)]
          [x y])))

(defn map-dimension [n screen-dim-max dimension-min dimension-max]
  (q/map-range n 0 screen-dim-max dimension-min dimension-max))

(defn screen-coord-mandel-point [x y mandel-x-min mandel-x-max mandel-y-min mandel-y-max]
  [(map-dimension x screen-width mandel-x-min mandel-x-max)
   (map-dimension y screen-height mandel-y-min mandel-y-max)])

(defn setup-state []
  {:x 0 :y 0 :n 0})

(defn next-position [x y width]
  (let [x' (inc x)
        wrapped-x (if (< x' width) x' 0)]
    [wrapped-x
     (if (zero? wrapped-x) (inc y) y)]))

(defn update-state [{x :x y :y :as state}])
  ; How are we going to pass in m/mandel-x-min etc?
  ; Are we going to have to dereference them from m/ every iteration?

  ; TODO: Grab queued pixels from m/, clear the queue, then update


(defn draw-state [{x :x y :y n :n}]
  (let [c (c/complex-purple n)]

    (q/with-stroke c
      (q/point x y))))

(defn -main []
  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :middleware [mi/fun-mode]))


