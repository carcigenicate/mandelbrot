(ns mandelbrot.serialization
  (:require [clojure.edn :as edn]))

(def test-save "[-0.1067121935483871 -0.1050301935483870 0.9250769874213836 0.9267589874213837]Date5.11832930629394E8[1.08 4.97 2.99 4.06 1.52 2.56 1.42 1.42 2.24]")

; TODO: The stringify counterpart is format-options in color-picker. Move-in.

(defn parse [^String save]
  (let [location (edn/read-string test-save)
        next-chunk (apply str (drop-while #(not= % \[) (drop 1 test-save)))
        colors (edn/read-string next-chunk)]))


