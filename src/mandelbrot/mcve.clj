(ns mandelbrot.mcve
  (:require [seesaw.core :as sc])
  (:import (java.awt.image BufferedImage)))

(defn make-huge-image []
  (BufferedImage. 20000 13333 BufferedImage/TYPE_INT_RGB))

(defn frame []
  (sc/invoke-soon
    @(future (make-huge-image))))
