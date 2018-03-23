(ns mandelbrot.limits)

(defrecord Mandelbrot-Limits [start-r end-r start-i end-i rep-width rep-height])

(defn repless-limits [start-r end-r start-i end-i]
  (->Mandelbrot-Limits start-r end-r start-i end-i -1 -1))