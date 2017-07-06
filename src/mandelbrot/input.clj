(ns mandelbrot.input
  (:require [mandelbrot.state :as s]
            [mandelbrot.concur-iter-finder :as cif]
            [quil.core :as q]))

(def move-perc (bigdec 0.5))
(def zoom-perc (bigdec 0.8))

(defn update-limit [state limit-key f]
  (update-in state [:mandel-limits limit-key] f))

; TODO: DRY

(defn move-x [state by]
  (-> state
      (update-limit :x-min #(+ % by))
      (update-limit :x-max #(+ % by))))

(defn move-y [state by]
  (-> state
      (update-limit :y-min #(+ % by))
      (update-limit :y-max #(+ % by))))

(defn limit-dimension-sizes [limits]
  (let [{:keys [x-min x-max y-min y-max]} limits]
    [(- x-max x-min)
     (- y-max y-min)]))

(defn zoom [state x-by y-by]
  (let []
    (-> state
        (update-limit :x-min #(+ % x-by))
        (update-limit :x-max #(- % x-by))

        (update-limit :y-min #(+ % y-by))
        (update-limit :y-max #(- % y-by)))))

(defn action-dispatch [key state]
  (let [[x-length y-length] (limit-dimension-sizes (:mandel-limits state))
        ; TODO: Why are the 0.5s necessary? Causes infinite to creep in if removed.
        x-zoom-adj (* x-length zoom-perc 0.5)
        x-move-adj (* x-length move-perc 0.5)
        y-zoom-adj (* y-length zoom-perc 0.5)
        y-move-adj (* y-length move-perc 0.5)]

    (case key
      \a (move-x state (- x-move-adj))
      \d (move-x state x-move-adj)

      \w (move-y state (- y-move-adj))
      \s (move-y state y-move-adj)

      \z (zoom state x-zoom-adj y-zoom-adj) ; In
      \x (zoom state (- x-zoom-adj) (- y-zoom-adj)) ; Out

      nil)))

(defn key-handler [state event]
  (let [{raw :raw-key} event
        effected-state (action-dispatch raw state)]
    (println "Key:" raw)
    (if effected-state
      (do
        (println (into {} (:mandel-limits effected-state)))
        (cif/cancel-finding-all)
        (s/populate-rows effected-state (q/width) (q/height)))

      state)))