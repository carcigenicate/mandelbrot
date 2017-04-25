(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]
            [mandelbrot.concur-iter-finder :as cif]

            [helpers.general-helpers :as g])

  (:gen-class))

(def screen-width 200)
(def screen-height 200)

(def max-tests 200)

(defrecord Mandel-Limits [x-min x-max y-min y-max])

(defn all-pixels-to-draw []
  (into #{}
        (for [y (range screen-height)
              x (range screen-width)]
          [x y])))

(defn map-dimension [n screen-dim-max dimension-min dimension-max]
  (q/map-range n 0 screen-dim-max dimension-min dimension-max))

(defn screen-coord-mandel-point [x y limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(map-dimension x screen-width x-min x-max)
     (map-dimension y screen-height y-min y-max)]))

(defn screen-points-to-mandel [screen-points limits]
  (map (fn [[x y]] (screen-coord-mandel-point x y limits))
       screen-points))

(defn setup-state []
  (let [points (all-pixels-to-draw)]
    (cif/start-finder points max-tests)
    {:mandel-limits (->Mandel-Limits -2 2 -2 2)}))

(defn update-state [state]
  ; How are we going to pass in m/mandel-x-min etc?
  ; Are we going to have to dereference them from m/ every iteration?

  ; TODO: Grab queued pixels from m/, clear the queue, then update
  state)


(defn draw-state [state]
  (let [point-data (cif/grab-and-clear-queue)]

    (doseq [{a :a b :b n :n} point-data]
      ; TODO: Need to translate a,b back to x,y to draw
      (q/with-stroke c (c/complex-purple n)
        (q/point x y)))))

(defn -main []
  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :middleware [mi/fun-mode]))


