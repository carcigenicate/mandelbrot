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
  (let [[r g b] [75 5 5]]
    [(c (+ b (* n n n)))
     (c (+ g (* n n)))
     (c (+ r n))]))

(defn moss [n]
  (let [[r g b] [25 50 25]]
    [(c (+ b (* n)))
     (c (+ g (* n n n)))
     (c (+ r (* n n)))]))

(defn simple-shape [n]
  (let [sub -200
        [r g b] [sub sub sub]]
    [(c (+ b (* n n)))
     (c (+ g (* n n)))
     (c (+ r (* n n)))]))


(defn trippy1 [n a b]
  (let [f w]
    [(f (* n a b 2))
     (f (* n a b 3))
     (f (* n a b 4))]))

(defn trippy2 [n a b]
  (let [f w]
    [(f (/ (* n a b) 2))
     (f (/ (* n a b) 3))
     (f (/ (* n a b) 4))]))

(defn series [n]
  (cond
    (zero? (rem n 3))
    (lava n)

    (zero? (rem n 4))
    (icy-electricity n)

    (zero? (rem n 4))
    (simple-shape n)

    :else
    (complex-purple n)))


