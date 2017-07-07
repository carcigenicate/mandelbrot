(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]
            [mandelbrot.viewport-state :as vs]
            [mandelbrot.input :as i]
            [mandelbrot.locations :as lo]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh])

  (:gen-class))

(set! *warn-on-reflection* true)

(def screen-ratio 0.68M) ; 0.68 ~= screen ratio

(def screen-width 1000M)
(def screen-height (* screen-width screen-ratio))

(def draw-pixels-per-frame (* screen-width screen-height 0.05M))

(def starting-position [0M 0M])

(def global-coloring-f
  "Should accept three arguments: [a b n]
  and return a of [r g b] to color the point as."
  (fn [a b n] (c/temp a b n)))

(defrecord Animation-State [viewport-state current-draw-position])

(def background-color [10 10 100])

(def caster bigdec)

(def starting-mandel-limits lo/save-map2)

(defn advance-screen-coord [x y width]
  (let [x' (inc x)
        new-line? (>= x' width)
        x'' (if new-line? 0 x')
        y' (if new-line? (inc y) y)]
    [x'' y']))

(defn test-pixel [a b]
  (m/standard-mandelbrot-test-convergence a b))

(defn draw-pixel [x y color]
  (q/with-stroke color
    (qh/with-weight 1
      (q/point x y))))

(defn draw-n-pixels
  "Draw n-pixels many pixels starting at starting-pos, moving left-right, top-down.
  Returns the position of the next pixel to be drawn."
  [n-pixels starting-pos view-state]
  ; TODO: Eww. a and b are expensive to compute, so I'd rather not have to do it in
  ; TODO:   test-pixel and this function. Causes bloat though.
  (reduce (fn [[acc-x acc-y] _]
            (let [[a b] (vs/screen-to-mandel acc-x acc-y view-state)
                  n (test-pixel a b)
                  color (global-coloring-f a b n)]
              (draw-pixel acc-x acc-y color)
              (advance-screen-coord acc-x acc-y screen-width)))

          starting-pos

          (range n-pixels)))

(defn new-viewport-state [mandel-limits]
  (vs/->Viewport-State (lo/cast-values-using caster mandel-limits)
                       (vs/new-zero-based-limits screen-width screen-height)))

(defn setup-state []
  (q/frame-rate 60)
  (apply q/background background-color)

  (let [view-state (new-viewport-state starting-mandel-limits)]
    (->Animation-State view-state starting-position)))

(defn update-state [state]
    (let [{[_ y :as starting-pos] :current-draw-position
           view-state :viewport-state} state]

      (if (< y screen-height)
        (do
          (println "Cur pos:" starting-pos)

          (assoc state :current-draw-position
                       (draw-n-pixels draw-pixels-per-frame
                                      starting-pos
                                      view-state)))

        state)))

(defn key-handler-wrapper [state event]
  state)

(defn mouse-handler [state event]
  (when state
    (apply q/background background-color)

    (-> state
      (update :viewport-state
              #(let [new-state (i/mouse-handler % event caster)]
                 (println (into {} (:mandel-limits new-state)))
                 new-state))


      (assoc :current-draw-position starting-position))))

(defn -main []

  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state

               :key-pressed key-handler-wrapper
               :mouse-pressed mouse-handler

               :middleware [mi/fun-mode]
                #_
               :on-close))
