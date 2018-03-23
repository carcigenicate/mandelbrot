(ns mandelbrot.serialization
  (:require [clojure.edn :as edn]

            [mandelbrot.limits :as ml]
            [mandelbrot.color-options :as co]))

(def test-save "[-0.1067121935483871 -0.1050301935483870 0.9250769874213836 0.9267589874213837]Date5.11832930629394E8[1.08 4.97 2.99 4.06 1.52 2.56 1.42 1.42 2.24].jpg")

; TODO: The stringify counterpart is format-options in color-picker. Move-in.

(defn parse?
  "Returns a pair of [save-location save-color-scheme], or nil if it can't parse the save."
  [^String save]
  (try
    (let [location (edn/read-string save) ; read-string reads the first object off the string
          colors (->> save
                      (drop 1) ; Drop the first opening bracket
                      (drop-while #(not= % \[)) ; Then drop until we reach the next vector
                      (apply str)
                      (edn/read-string))]
      [(apply ml/repless-limits location)
       (apply co/new-color-options colors)])

    (catch RuntimeException e
      ; If location or colors is anything other than a collection, apply will fail with an IllegalArgumentException
      ; If there are unmatched brackets, it will fail with a general RuntimeException
      nil)))


