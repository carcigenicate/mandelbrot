(ns mandelbrot.mandel-point)

(defrecord Mandel-Point [x y a b n])

(defn rows-of [row-width n-rows]
  (for [y (range n-rows)
        x (range row-width)]
    [x y]))