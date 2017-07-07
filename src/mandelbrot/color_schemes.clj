(ns mandelbrot.color-schemes
  (:require [helpers.general-helpers :as g]))

; TODO: All litarals should be big decimals to prevent precision loss.

(defn c [n]
  (g/clamp n 0M 255M))

(defn w [n]
  (g/wrap n 0M 255M))

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
    [(c (+ r (* n n n)))
     (c (+ g (* n n)))
     (c (+ b n))]))

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

(defn test1 [n a b]
  [(w (* n n))
   (w (* n a))
   (w (* n b))])

(defn series [n]
  (cond
    (zero? (rem n 3M))
    (lava n)

    (zero? (rem n 4M))
    (icy-electricity n)

    (zero? (rem n 4M))
    (simple-shape n)

    :else
    (complex-purple n)))

(defn wut [a b n]
  [(w (* n a -1))
   (w (* n b -1))
   (w (* n a b 500M))])

(defn temp [a b n]
  [(w (Math/pow a n))
   (c (* b n))
   (c (* a b n -100))])



