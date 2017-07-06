(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]
            [mandelbrot.concur-iter-finder :as cif]
            [mandelbrot.state :as s]
            [mandelbrot.input :as i]
            [mandelbrot.locations :as l]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh])

  (:import [java.math MathContext RoundingMode])

  (:gen-class))

(set! *warn-on-reflection* true)

(set! *math-context* (MathContext. 10 RoundingMode/HALF_EVEN))

(def screen-ratio 0.68) ; 0.68 ~= screen ratio

(def screen-width 1500)
(def screen-height (* screen-width screen-ratio))

(def max-tests 50)

(def background-color [10 10 100])

(def starting-limits (s/map->Mandel-Limits
                       (l/cast-values-using bigdec l/full-map)))

(defn setup-state []
  (q/frame-rate 100)
  (apply q/background background-color)

  (let [points (s/mandel-row-partitions starting-limits screen-width screen-height)]

    (s/->State starting-limits points)
    #_
    (s/initial-state points)))

(defn update-state [state]
  (let [{limits :mandel-limits rows :rows} state
        [row & rest-rows] rows]

    (when row
      (cif/start-finding row max-tests))

    (assoc state :rows rest-rows)))

(defn draw-state [state]
  (let [{limits :mandel-limits} state
        point-data (cif/grab-and-clear-queue)]

    (qh/with-weight 1
      (doseq [{a :a b :b n :n :as point} point-data]
        (let [[x y] (s/mandel-point-to-screen-point a b screen-width screen-height limits)
              color (c/temp a b n)]
          (q/with-stroke color
            (q/point x y)))))))

(defn key-handler-wrapper [state event]
  (let [state' (i/key-handler state event)
        limits (:mandel-limits state)
        limits' (:mandel-limits state')]
    (if-not (= limits limits')
      (do
        (apply q/background background-color)
        state')
      state)))


(defn -main []

  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :key-pressed key-handler-wrapper

               :middleware [mi/fun-mode]

               :on-close
               #(do % (println "Running closing routine...") (cif/cancel-finding-all))))


