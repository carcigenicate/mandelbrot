(ns mandelbrot.concurrent-finder
  (:require [mandelbrot.mandelbrot-iteration :as mi]

            [clojure.core.async :refer [>! chan thread go put!]]

            [helpers.general-helpers :as g]))

(defrecord Mandelbrot-Limits [start-r end-r start-i end-i rep-width rep-height])

(defn generate-check-points
  "Returns a lazy list of [r i rx ry], where r and i are points on the complex plane, and rx ry are the corresponding points in the representation."
  [start-r end-r start-i end-i rep-width rep-height]
  (let [field-width (- end-r start-r)
        field-height (- end-i start-i)
        x-step (double (/ field-width rep-width))
        y-step (double (/ field-height rep-height))]

    (for [i (range start-i end-i y-step)
          r (range start-r end-r x-step)
          :let [rx (g/map-range r start-r end-r 0 rep-width)
                ry (g/map-range i start-i end-i 0 rep-height)]]

      [r i rx ry])))

(defn calc-iter-point [r i rep-x rep-y]
  (mi/->Point r i rep-x rep-y (mi/standard-mandelbrot-test-convergence r i)))

; Slightly faster with smaller draws. Took 75s vs 12s
(defn future-rows [start-x end-x start-y end-y rep-width rep-height]
  (let [field-width (- end-x start-x)
        field-height (- end-y start-y)
        x-step (double (/ field-width rep-width))
        y-step (double (/ field-height rep-height))]

    (for [y (range start-y end-y y-step)]
      (future
        (vec
          (for [x (range start-x end-x x-step)
                :let [rx (g/map-range x start-x end-x 0 rep-width)
                      ry (g/map-range y start-y end-y 0 rep-height)]]

            (mi/->Point x y rx ry (mi/standard-mandelbrot-test-convergence x y))))))))

(defn future-rows2 [start-r end-r start-i end-i rep-width rep-height]
  (let [pts (generate-check-points start-r end-r start-i end-i rep-width rep-height)
        total (* rep-width rep-height)
        chunk-size (int (/ total 4))
        parted (partition chunk-size pts)]

    (mapv
      (fn [chunk] (future
                    (mapv #(apply calc-iter-point %) chunk)))
      parted)))

(defn generated-find-point-chan [percent-divisions limits]
  (let [{:keys [start-r end-r start-i end-i rep-width rep-height]} limits
        parts (int (* rep-width rep-height percent-divisions 0.5))
        out-chan (chan parts)] ; Not sure what size of buffer is appropriate. Too big?

    (go
      (let [pts (generate-check-points start-r end-r start-i end-i rep-width rep-height)
            parted (partition parts pts)]

        (doseq [[i chunk] (map vector (range) parted)]
          (go
            (let [processed-chunk (mapv #(apply calc-iter-point %) chunk)]
              (println "Finished" i)
              (>! out-chan processed-chunk))))))


    out-chan))

