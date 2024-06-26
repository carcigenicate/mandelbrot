(ns mandelbrot.concurrent-finder
  (:require [mandelbrot.mandelbrot-iteration :as mi]

            [clojure.core.async :refer [>! >!! chan thread go put! go-loop close!]]

            [mandelbrot.helpers.general-helpers :as g]))

(defn generate-check-points
  "Returns a lazy list of [r i rx ry], where r and i are points on the complex plane, and rx ry are the corresponding points in the representation."
  [limits]
  (let [{:keys [start-r end-r start-i end-i rep-width rep-height]} limits
        field-width (- end-r start-r)
        field-height (- end-i start-i)
        x-step (double (/ field-width rep-width))
        y-step (double (/ field-height rep-height))

        ; Needed or else map-range returns values greater than rep-width if
        ;  rep-width is a Ratio
        [floored-width floored-height] [(int rep-width) (int rep-height)]]

    #_ ; With widths of 45, 90, or 180, distortions appear in the image
    (println "Field w/h" [field-width field-height] "\n"
             "Steps h/v" [x-step y-step] "\n"
             "Floored w/h" [floored-width floored-height])

    (for [i (range start-i end-i y-step)
          r (range start-r end-r x-step)
          :let [rx (g/map-range r start-r end-r 0 floored-width)
                ry (g/map-range i start-i end-i 0 floored-height)]]

      [r i rx ry])))

(defn calc-iter-point [r i rep-x rep-y]
  (mi/->Point r i rep-x rep-y (mi/standard-mandelbrot-test-convergence r i)))

(defn lazy-calc-points [limits]
  (map (partial apply calc-iter-point) (generate-check-points limits)))

(defn lazy-par-calc-points [limits]
  (pmap (partial apply calc-iter-point) (generate-check-points limits)))

(defn lazy-par-chunks [perc-divisions limits]
  (let [{:keys [rep-width rep-height]} limits
        chunk-size (int (* rep-width rep-height perc-divisions))
        points (generate-check-points limits)]

    (pmap (fn [chunk]
            (mapv (partial apply calc-iter-point) chunk))
          (partition chunk-size points))))

(defn future-rows [limits]
  (let [{:keys [start-r end-r start-i end-i rep-width rep-height]} limits
        field-width (- end-r start-r)
        field-height (- end-i start-i)
        x-step (double (/ field-width rep-width))
        y-step (double (/ field-height rep-height))]

    (for [y (range start-i end-i y-step)]
      (future
        (vec
          (for [x (range start-r end-r x-step)
                :let [rx (g/map-range x start-r end-r 0 rep-width)
                      ry (g/map-range y start-i end-i 0 rep-height)]]

            (mi/->Point x y rx ry (mi/standard-mandelbrot-test-convergence x y))))))))

(defn future-rows2 [limits]
  (let [{:keys [rep-width rep-height]} limits
        pts (generate-check-points limits)
        total (* rep-width rep-height)
        chunk-size (int (/ total 4))
        parted (partition chunk-size pts)]

    (mapv
      (fn [chunk] (future
                    (mapv #(apply calc-iter-point %) chunk)))
      parted)))

(defn generated-find-point-chan [percent-divisions limits]
  (let [{:keys [rep-width rep-height]} limits
        parts (int (* rep-width rep-height percent-divisions 0.5))
        out-chan (chan parts)]

    (go
      (let [pts (generate-check-points limits)
            parted (partition parts pts)]

        (doseq [[i chunk] (map vector (range) parted)]
          (go
            (let [processed-chunk (mapv #(apply calc-iter-point %) chunk)]
              (println "Finished" i)
              (>! out-chan processed-chunk))))))

    out-chan))

(defn calc-loop
  "Terminates as soon as the running!? atom is read as false.
  Returns [] if terminated, otherwise it returns the calculated chunk."
  [running!? chunk]
  (reduce (fn [acc point]
            (if @running!?
              (conj acc (apply calc-iter-point point))
              (reduced [])))
          []
          chunk))

(defn point-calculator-component
  "Divides the screen into parts, each percent-divisions large (with a percent of 0.5, there will be 2 divisions),
  and calculates all the points for each chunk.

  Returns a pair of [result-chan stop-f]
  Where result-chan is a channel that supplies the calculated chunks, and stop-f is a 0-arity function that stops further
  calculations from happening when called, and closes result-chan."
  [percent-divisions limits]
  (let [{:keys [rep-width rep-height]} limits
        chunk-size (int (* rep-width rep-height percent-divisions))
        out-chan (chan chunk-size)
        running!? (atom true)
        stop-f #(do (reset! running!? false)
                    (close! out-chan))]

    (go
      (let [pts (generate-check-points limits)
            parted (partition chunk-size pts)
            pending-jobs! (atom (into #{} (range (count parted))))]

        (doseq [[i chunk] (map vector (range) parted)]
          (thread
            (let [processed-chunk (calc-loop running!? chunk)]
              #_(println "Finished" (inc i) "/" (count parted))
              (>!! out-chan processed-chunk)
              (swap! pending-jobs! #(disj % i))

              (when (empty? @pending-jobs!)
                (stop-f)
                #_(println "Done. Stopped.")))))))

    [out-chan stop-f]))
