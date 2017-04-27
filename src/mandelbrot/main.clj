(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]
            [mandelbrot.concur-iter-finder :as cif]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh])

  (:gen-class))

(set! *warn-on-reflection* true)

(def screen-ratio 1) ; ~0.68 = screen ratio

(def screen-width 1500)
(def screen-height (* screen-width screen-ratio))

(def max-tests 200)

(def key-move-increment 0.25)
(def key-zoom-increment 0.5)

(defrecord Mandel-Limits [x-min x-max y-min y-max])

(def initial-limits (->Mandel-Limits -2 2 -2 2))

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

(defn pixel-row-partitions [width height]
  (for [x (range width)]
    (for [y (range height)]
      [x y])))

(defn mandel-row-partitions [limits screen-width screen-height]
  (let [rows (pixel-row-partitions screen-width screen-height)]
     (map #(screen-points-to-mandel % limits) rows)))

(defn populate-rows [state]
  (assoc state :rows
               (mandel-row-partitions (:mandel-limits state)
                                      screen-width screen-height)))

(defn setup-state []
  (q/frame-rate 100)
  (q/background 10 10 100)

  (let [points (mandel-row-partitions initial-limits screen-width screen-height)]

    {:mandel-limits initial-limits
     :rows points
     :updated? false}))

(defn update-state [state]
  (let [{limits :mandel-limits rows :rows} state
        [row & rest-rows] rows]

    (cif/start-finding row max-tests)

    (assoc state :rows rest-rows)))

(defn draw-state [state]
  (let [{limits :mandel-limits} state
        point-data (cif/grab-and-clear-queue)]

    (qh/with-weight 1
      (doseq [{a :a b :b n :n :as point} point-data]
        (let [[x y] (mandel-point-to-screen-point a b limits)
              color (c/icy-electricity n)]
          (q/with-stroke color
            (q/point x y)))))))

(defn key-handler [state event]
  (println state)
  (let []))

(defn -main []

  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :key-pressed key-handler

               :middleware [mi/fun-mode]))


