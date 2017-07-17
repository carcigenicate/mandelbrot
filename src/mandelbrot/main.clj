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

(def screen-width 300M)

(def starting-mandel-limits lo/branch-out)

(def finding-divisions 6)

(def global-coloring-f
  "Should accept three arguments: [a b n]
  and return a of [r g b] to color the point as."
  (fn [a b n] (c/poster-replicate a b n)))

; ---------- MAIN SETTINGS ----------

(def screen-ratio 0.68M) ; 0.68 ~= screen ratio
(def screen-height (* screen-width screen-ratio))

(def pixels-per-chunk (with-precision 10
                        (int (/ (* screen-width screen-height) finding-divisions))))

(defrecord Animation-State [viewport-state collector coord-chunks])

(def background-color [10 10 100])

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

(defn generate-task-chunks [coords viewport-state]
  (let [tasks (generate-tasks-for coords viewport-state)]
    (partition pixels-per-chunk tasks)))

(defn new-chunked-tasks [viewport-state]
  (let [coords (mp/rows-of screen-width screen-height)]
    (generate-task-chunks coords viewport-state)))

(defn setup-state []
  (q/frame-rate 20)

  (apply q/background background-color)

  (let [view-state (new-viewport-state starting-mandel-limits)
        collector  (fr/new-result-collector)
        chunks     (new-chunked-tasks view-state)
        state      (->Animation-State view-state collector chunks)]

    (println "Done setup.")

    state))

(defn update-state [state]
  (let [{[chunk & rest-chunks] :coord-chunks
         collector :collector
         view-state :viewport-state} state]

    (fr/start-tasks collector chunk)

    (assoc state :coord-chunks rest-chunks)))

(defn draw-state [state]
  (let [{collector :collector vs :viewport-state} state
        results (fr/get-results collector)]
    #_
    (println "Drawing" (count results))

    (doseq [{:keys [x y a b n]} results]
      (draw-pixel x y
                  (global-coloring-f a b n)))))

(defn key-handler-wrapper [state event]
  state) ; TODO: Write

(defn mouse-handler [state event]
  (fr/cancel-tasks (:collector state))

  (when state
    (apply q/background background-color)

    (let [new-viewport (i/mouse-handler (:viewport-state state) event)]
      (println (into {} (:mandel-limits new-viewport)))

      (-> state
          (assoc :viewport-state new-viewport)
          (assoc :coord-chunks (new-chunked-tasks new-viewport))
          (assoc :collector (fr/new-result-collector))))))

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

               :on-close (fn [s]
                           (fr/cancel-tasks (:collector s))
                           (System/gc)
                           (println "Cleanup routine run."))))
