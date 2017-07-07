(ns mandelbrot.input
  (:require [mandelbrot.viewport-state :as vs]
            [quil.core :as q]))

(def zoom-perc 0.9)

(defn new-limits-centered-on [a b limit-width limit-height num-type-caster]
  (let [hw (/ limit-width 2M)
        hh (/ limit-height 2M)
        c num-type-caster]

    (vs/->Limits (c (- a hw)) (c (+ a hw))
                 (c (- b hh)) (c (+ b hh)))))

(defn zoom-to [mandel-limits zoom-by]
  (-> mandel-limits
    (update :x-min #(+ % zoom-by))
    (update :x-max #(- % zoom-by))
    (update :y-min #(+ % zoom-by))
    (update :y-max #(- % zoom-by))))

(defn mouse-handler [view-state event type-caster]
  (let [{x :x, y :y, button :button} event
        [a b] (vs/screen-to-mandel x y view-state)]

    (with-precision vs/mapping-precision
      (update view-state :mandel-limits
              #(let [[x-length y-length] (vs/limit-dimension-sizes %)
                     new-limits (new-limits-centered-on a b x-length y-length type-caster)

                     x-zoom-adj (type-caster (* x-length zoom-perc 0.5M))
                     x-zoom-adj' (if (= button :left) x-zoom-adj (- x-zoom-adj))]

                 (zoom-to new-limits x-zoom-adj'))))))
