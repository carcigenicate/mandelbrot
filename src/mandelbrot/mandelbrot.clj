(ns mandelbrot.mandelbrot
  (:require [clojure.test :refer [is]]))

(def std-infinity-limit 4M)
(def std-n-tests 200M)

(defn square-complex [a b]
  [(- (* a a)
      (* b b))

   (* 2M a b)])

(defn mandelbrot-iteration-f [seed-real seed-imag last-real last-imag]
  (let [[new-real new-imag] (square-complex last-real last-imag)]
    [(+ new-real seed-real)
     (+ new-imag seed-imag)]))

(defn test-point-convergence [real imag max-iterations infinity-limit iteration-f]
  #_
  {:pre [(is (decimal? real) (decimal? imag))]
   :post [(is (decimal? %))]}
  (let [limit-sqrd (* infinity-limit infinity-limit)

        under-limit? #(<= (+ (* % %)
                             (* %2 %2))
                          limit-sqrd)]
    (loop [real-acc real
           imag-acc imag
           iter 0M]

      (if (and (< iter max-iterations)
               (under-limit? real-acc imag-acc))

        (let [[new-real new-imag] (iteration-f real-acc imag-acc)]
          (recur new-real new-imag (inc iter)))

        iter))))

(defn test-point-convergence-timing-test [real imag max-iterations infinity-limit iteration-f]
  (let [limit-sqrd (* infinity-limit infinity-limit)

        under-limit? #(<= (+ (* % %)
                             (* %2 %2))
                          limit-sqrd)]
    (loop [real-acc real
           imag-acc imag
           iter 0M]

      (if (< iter max-iterations)

        (let [[new-real new-imag] (iteration-f real-acc imag-acc)]
          (recur new-real new-imag (inc iter)))

        iter))))

(defn standard-mandelbrot-test-convergence [real imag]
  (test-point-convergence real imag
                          std-n-tests std-infinity-limit
                          (partial mandelbrot-iteration-f real imag)))






