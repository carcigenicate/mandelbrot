(ns mandelbrot.color-schemes
  (:require [helpers.general-helpers :as g]))

(defn- int-to-color [n]
  (let [r (int (/ n (* 255 255 255)))
        n' (rem n 255)

        g (int (/ n' (* 255 255)))
        n'' (rem n' 255)

        b (int (/ n'' 255))]
    [r g b]))

(defn complex-purple [n]
  (let [[r g b] [100 50 25]
        w #(g/wrap % 0 255)]
    [(w (+ r n))
     (w (+ g (* n n)))
     (w (+ b (* n n n)))]))

(defn complex-purple2 [n]
  (let [[r g b] [100 50 25]
        w #(g/clamp % 0 255)]
    [(w (+ r n))
     (w (+ g (* n n)))
     (w (+ b (* n n n)))]))