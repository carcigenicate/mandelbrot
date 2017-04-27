(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]
            [mandelbrot.concur-iter-finder :as cif]
            [mandelbrot.state :as s]
            [mandelbrot.input :as i]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh])

  (:gen-class))

(set! *warn-on-reflection* true)

(def screen-ratio 1) ; ~0.68 = screen ratio

(def screen-width 1000)
(def screen-height (* screen-width screen-ratio))

(def max-tests 200)

(defn setup-state []
  (q/frame-rate 100)
  (q/background 10 10 100)

  (let [points (s/mandel-row-partitions s/initial-limits screen-width screen-height)]

    (s/initial-state points)))

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
        (let [[x y] (s/mandel-point-to-screen-point a b screen-width screen-height limits)
              color (c/icy-electricity n)]
          (q/with-stroke color
            (q/point x y)))))))

(defn -main []

  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :key-pressed i/key-handler

               :middleware [mi/fun-mode]))


