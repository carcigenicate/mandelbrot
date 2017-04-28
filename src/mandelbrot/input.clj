(ns mandelbrot.input
  (:require [mandelbrot.state :as s]
            [mandelbrot.concur-iter-finder :as cif]
            [quil.core :as q]))

(def key-move-increment 0.25)
(def key-zoom-increment 0.5)

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

;TODO Need to divide?
(defn zoom [state by]
  (-> state
    (update-limit :x-min #(+ % by))
    (update-limit :y-min #(+ % by))

    (update-limit :x-max #(- % by))
    (update-limit :y-max #(- % by))))

(defn action-dispatch [key state]

  (let [m key-move-increment
        z key-zoom-increment]
    (case key
      \a (move-x state (- m))
      \d (move-x state m)

      \w (move-y state (- m))
      \s (move-y state m)

      \z (zoom state z) ; In
      \x (zoom state (- z)) ; Out

      nil)))

(defn key-handler [state event]
  (let [{raw :raw-key} event
        effected-state (action-dispatch raw state)]
    (println "Key:" raw)
    (if effected-state
      (do
        (println (str (:mandel-limits effected-state)))
        (cif/cancel-finding-all)
        (s/populate-rows effected-state (q/width) (q/height)))

      state)))