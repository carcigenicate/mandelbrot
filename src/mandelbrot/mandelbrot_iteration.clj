(ns mandelbrot.mandelbrot-iteration
  (:require [helpers.general-helpers :as g]))

(def std-infinity-limit 2)
(def std-n-tests 200)

(defrecord Point [r i rep-x rep-y iters])

(defn square-complex [r i]
  [(- (* r r)
      (* i i))

   (* 2 r i)])

(defn mandelbrot-iteration-f [seed-real seed-imag last-real last-imag]
  (let [[new-real new-imag] (square-complex last-real last-imag)]
    [(+ new-real seed-real)
     (+ new-imag seed-imag)]))

(defn test-point-convergence [real imag max-iterations infinity-limit]
  (let [limit-sqrd (* infinity-limit infinity-limit)
        iter-f (partial mandelbrot-iteration-f real imag)

        under-limit? #(<= (+ (* % %)
                             (* %2 %2))
                          limit-sqrd)]
    (loop [real-acc real
           imag-acc imag
           iter 0]

      (if (and (< iter max-iterations)
               (under-limit? real-acc imag-acc))

        (let [[new-real new-imag] (iter-f real-acc imag-acc)]
          (recur new-real new-imag (inc iter)))

        iter))))

(defn standard-mandelbrot-test-convergence [real imag]
  (test-point-convergence real imag std-n-tests std-infinity-limit))

(defn iteration-counts-matrix [start-x end-x start-y end-y rep-width rep-height]
  (let [field-width (- end-x start-x)
        field-height (- end-y start-y)
        x-step (double (/ field-width rep-width))
        y-step (double (/ field-height rep-height))]

    (vec
      (for [y (range start-y end-y y-step)]
        (vec
          (for [x (range start-x end-x x-step)
                :let [rx (g/map-range x start-x end-x 0 rep-width)
                      ry (g/map-range y start-y end-y 0 rep-height)]]

            (->Point x y rx ry (standard-mandelbrot-test-convergence x y))))))))


