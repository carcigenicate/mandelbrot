(ns mandelbrot.main
  (:require [mandelbrot.image-producer.producer :as mp])
  (:gen-class))

(defn -main []
  (println "...")
  (read-line)

  (mp/test-routine))