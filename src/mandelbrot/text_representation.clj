(ns mandelbrot.text-representation
  (:require [mandelbrot.mandelbrot-iteration :as mi]
            [clojure.string :as s]
            [helpers.general-helpers :as g]))

(defn iters-rep [iters]
  (condp <= iters
    200 \#
    75 \%
    25 \!
    10 \^
    0 \space))

(defn format-iteration-matrix [count-matrix]
  (s/join "\n"
          (map (fn [row]
                 (s/trimr (apply str (map (comp iters-rep :iters) row))))
               count-matrix)))

(defn pprint [count-matrix]
  (println
    (format-iteration-matrix count-matrix)))
