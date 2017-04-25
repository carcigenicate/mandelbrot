(ns mandelbrot.mandelbrot)

(defn square-complex [a b]
  [(- (* a a)
      (* b b))

   (* 2 a b)])

(defn fz=z2+c [[zr zi] [cr ci]]
  (let [[r' i'] (square-complex zr zi)]
    [(+ r' cr) (+ i' ci)]))

; TODO: Terrible name?
(defn converges-at? [a b max-iters]
  (loop [n 0
         r a
         i b]
    (let [[r' i'] (fz=z2+c [r i] [a b])]
      (if (and (< n max-iters)
               (<= (Math/abs ^double (+ r' i')) 2))
        (recur (inc n) r' i')
        n))))