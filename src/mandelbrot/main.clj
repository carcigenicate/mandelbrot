(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]
            [mandelbrot.concur-iter-finder :as cif]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh])

  (:gen-class))

(def screen-ratio 0.6)

(def screen-width 2500)
(def screen-height (* screen-width screen-ratio))

(def max-tests 100)

(defrecord Mandel-Limits [x-min x-max y-min y-max])

(defn all-pixels-to-draw []
  (vec
    (for [y (range screen-height)
          x (range screen-width)]
      [x y])))

(defn map-dimension [n screen-dim-max dimension-min dimension-max]
  (q/map-range n 0 screen-dim-max dimension-min dimension-max))

(defn screen-coord-to-mandel-point [x y limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(map-dimension x screen-width x-min x-max)
     (map-dimension y screen-height y-min y-max)]))

(defn mandel-point-to-screen-point [a b limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(Math/round ^float (q/map-range a x-min x-max 0 screen-width))
     (Math/round ^float (q/map-range b y-min y-max 0 screen-height))]))

(defn screen-points-to-mandel [screen-points limits]
  (map (fn [[x y]] (screen-coord-to-mandel-point x y limits))
       screen-points))

(defn setup-state []
  (q/frame-rate 1)

  (let [points (all-pixels-to-draw)
        limits (->Mandel-Limits -2 2 -2 2)
        mapped-points (screen-points-to-mandel points limits)]
    (cif/start-finder mapped-points max-tests)
    (println "Setup Finished...")
    {:mandel-limits limits}))

(defn update-state [state]
  ; How are we going to pass in m/mandel-x-min etc?
  ; Are we going to have to dereference them from m/ every iteration?

  ; TODO: Grab queued pixels from m/, clear the queue, then update
  state)

(defn draw-state [state]
  (let [{limits :mandel-limits} state
        point-data (cif/grab-and-clear-queue)]
    #_
    (clojure.pprint/pprint point-data)

    (qh/with-weight 1
      (doseq [{a :a b :b n :n :as point} point-data]
        (let [[x y] (mandel-point-to-screen-point a b limits)
              c (c/complex-purple n)]
          (q/with-stroke c
            (q/point x y)))))))

(defn -main []
  (println "Press enter to start...")
  (read-line)

  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :middleware [mi/fun-mode]))


