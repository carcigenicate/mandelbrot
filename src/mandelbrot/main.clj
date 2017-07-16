(ns mandelbrot.main
  (:require [clojure.test :refer [is]]

            [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]
            [mandelbrot.viewport-state :as vs]
            [mandelbrot.input :as i]
            [mandelbrot.locations :as lo]
            [mandelbrot.mandel-point :as mp]
            [mandelbrot.future-results :as fr]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh])

  (:import [java.util Date])

  (:gen-class))


(set! *warn-on-reflection* true)

; ---------- MAIN SETTINGS ----------

(def screen-width 2000M)

(def starting-mandel-limits lo/full-map)

(def global-coloring-f
  "Should accept three arguments: [a b n]
  and return a of [r g b] to color the point as."
  (fn [a b n] (c/poster-replicate a b n)))

; ---------- MAIN SETTINGS ----------

(def screen-ratio 0.68M) ; 0.68 ~= screen ratio
(def screen-height (* screen-width screen-ratio))

(def pixels-per-chunk (int (* screen-width screen-height 0.01M)))

(defrecord Animation-State [viewport-state results-coll coord-chunks])

(def background-color [10 10 100])

(defn advance-screen-coord [x y width]
  (let [x' (inc x)
        new-line? (>= x' width)
        x'' (if new-line? 0M x')
        y' (if new-line? (inc y) y)]

    [x'' y']))

(defn test-pixel [a b]
  (m/standard-mandelbrot-test-convergence a b))

(defn draw-pixel [x y color]
  (q/with-stroke color
    (qh/with-weight 1
      (q/point x y))))

(defn new-viewport-state [mandel-limits]
  (vs/->Viewport-State (lo/cast-values-using bigdec mandel-limits)

                       (lo/cast-values-using bigdec
                         (vs/new-zero-based-limits screen-width screen-height))))

(defn generate-tasks-for [coords viewport-state]
  (map (fn [[x y]]
           #(with-precision vs/mapping-precision
              (let [[a b] (vs/screen-to-mandel x y viewport-state)
                    n (test-pixel a b)]
                (mp/->Mandel-Point x y a b n))))
       coords))

(defn setup-state []
  (q/frame-rate 60)

  (apply q/background background-color)

  (let [view-state (new-viewport-state starting-mandel-limits)
        results-coll (fr/new-result-container)
        coords (mp/rows-of screen-width screen-height)
        tasks (generate-tasks-for coords view-state)
        chunks (partition pixels-per-chunk tasks)
        state (->Animation-State view-state results-coll chunks)]

    (println "Done setup.")

    state))

(defn update-state [state]
  (let [{[chunk & rest-chunks] :coord-chunks
         results-coll :results-coll
         view-state :viewport-state} state]
    #_
    (println "Starting" (count chunk) "tasks." (count rest-chunks) "chunks remaining.")

    (fr/start-tasks chunk results-coll)

    (assoc state :coord-chunks rest-chunks)))

(defn draw-state [state]
  (let [{results-coll :results-coll vs :viewport-state} state
        results (fr/flush-results results-coll)]

    (println "Drawing" (count results))

    (doseq [{:keys [x y a b n]} results]
      (draw-pixel x y
                  (global-coloring-f a b n)))))

(defn key-handler-wrapper [state event]
  state) ; TODO: Write

(defn mouse-handler [state event]
  (when state
    (apply q/background background-color)

    (-> state
      (update :viewport-state
              #(let [new-state (i/mouse-handler % event)]
                 (println (into {} (:mandel-limits new-state)))
                 new-state)))))

(defn -main []
  (println "E:")
  (read-line)

  (q/defsketch Mandel
               :size [screen-width screen-height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :key-pressed key-handler-wrapper
               :mouse-pressed mouse-handler

               :middleware [mi/fun-mode]
                #_
               :on-close))
