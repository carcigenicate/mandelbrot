(ns mandelbrot.complex)

(defrecord Complex-Number [real imaginary]
  Object
  (toString [{r :real i :imaginary}] (str r "+" i "i")))

(defn complex-number [real imaginary]
  (->Complex-Number real imaginary))

(defn square [{a :real b :imaginary}]
  (complex-number
    (- (* a a)
       (* b b))

    (* 2 a b)))
