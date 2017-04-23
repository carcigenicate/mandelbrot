(ns mandelbrot.color-schemes
  (:require [helpers.general-helpers :as g]))

(defn complex-purple [n]
  (let [[r g b] [100 50 25]
        w #(g/wrap % 0 255)]
    [(w (+ r n))
     (w (+ g (* n n)))
     (w (+ b (* n n n)))]))