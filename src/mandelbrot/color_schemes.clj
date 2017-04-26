(ns mandelbrot.color-schemes
  (:require [helpers.general-helpers :as g]))

(defn c [n]
  (g/clamp n 0 255))

(defn w [n]
  (g/wrap n 0 255))

(defn- int-to-color [n]
  (let [r (int (/ n (* 255 255 255)))
        n' (rem n 255)

        g (int (/ n' (* 255 255)))
        n'' (rem n' 255)

        b (int (/ n'' 255))]
    [r g b]))

(defn complex-purple [n]
  (let [[r g b] [100 50 25]]
    [(w (+ r n))
     (w (+ g (* n n)))
     (w (+ b (* n n n)))]))

(defn icy-electricity [n]
  (let [[r g b] [25 25 50]]
    [(c (+ r n))
     (c (+ g (* n n)))
     (c (+ b (* n n n)))]))

(defn lava [n]
  (let [[r g b] [25 25 50]]
    [(c (+ b (* n n n)))
     (c (+ g (* n n)))
     (c (+ r n))]))

(defn simple-shape [n]
  (let [sub -200
        [r g b] [sub sub sub]]
    [(c (+ b (* n n)))
     (c (+ g (* n n)))
     (c (+ r (* n n)))]))


(defn quads [n a b]
  (let [f w]
    [(f (/ n a (if (zero? b) 0.1 b)))
     (f (* n a b))
     (f (+ n a b))]))