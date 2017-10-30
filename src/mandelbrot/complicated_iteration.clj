(ns mandelbrot.complicated-iteration)

(def std-infinity-limit 2)
(def std-n-tests 200)

(defn square-complex [a b]
  [(- (* a a)
      (* b b))

   (* 2 a b)])

(defn mandelbrot-iteration-f [seed-real seed-imag last-real last-imag]
  (let [[new-real new-imag] (square-complex last-real last-imag)]
    [(+ new-real seed-real)
     (+ new-imag seed-imag)]))

(defn test-point-convergence [real imag max-iterations infinity-limit iteration-f]
  (let [limit-sqrd (* infinity-limit infinity-limit)

        under-limit? #(<= (+ (* % %)
                             (* %2 %2))
                          limit-sqrd)]
    (loop [real-acc real
           imag-acc imag
           iter 0]

      (if (and (< iter max-iterations)
               (under-limit? real-acc imag-acc))

        (let [[new-real new-imag] (iteration-f real-acc imag-acc)]
          (recur new-real new-imag (inc iter)))

        iter))))

(defn test-point-convergence-timing-test [real imag max-iterations infinity-limit iteration-f]
  ;  Not sure why this is necessary
  (let [limit-sqrd (* infinity-limit infinity-limit)

        under-limit? #(<= (+ (* % %)
                             (* %2 %2))
                          limit-sqrd)]
    (loop [real-acc real
           imag-acc imag
           iter 0]

      (if (< iter max-iterations)
        (let [[new-real new-imag] (iteration-f real-acc imag-acc)]
          (recur new-real new-imag (inc iter)))

        iter))))

(defn standard-mandelbrot-test-convergence [real imag]
  (test-point-convergence real imag
                          std-n-tests std-infinity-limit
                          (partial mandelbrot-iteration-f real imag)))

(defn iteration-counts-matrix [start-x start-y end-x end-y rep-width rep-height]
  (let [field-width (- end-x start-x)
        field-height (- end-y start-y)
        x-step (double (/ field-width rep-width))
        y-step (double (/ field-height rep-height))]

    (for [y (range start-y end-y y-step)]
      (for [x (range start-x end-x x-step)]
        (standard-mandelbrot-test-convergence x y)))))


