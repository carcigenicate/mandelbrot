(ns mandelbrot.main
  (:require [mandelbrot.seesaw.third-try :as ui]
            [seesaw.core :as sc])
  (:gen-class))

(defn -main []
  (-> (ui/frame)
      (sc/show!)))