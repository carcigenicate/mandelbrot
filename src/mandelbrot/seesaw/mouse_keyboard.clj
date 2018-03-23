(ns mandelbrot.seesaw.mouse-keyboard
  (:require [seesaw.core :as sc]
            [mandelbrot.image-producer.producer :as mp]
            [mandelbrot.seesaw.helpers :as sh])
  (:import (java.awt.event KeyEvent)))
#_ ; Canvas saver needs arguments fixed
(defn save-handler [root height-ratio color-f limits _]
  (println "Saving...")
  (let [prog-bar (sc/select root [:#save-progress])
        slider (sc/select root [:#save-width-slider])
        width (sc/value slider)
        height (* width height-ratio)]

    (future
      (mp/canvas-saver prog-bar color-f
                       (assoc limits :rep-width width,
                                     :rep-height height))
      (println "Saved.")
      (sc/invoke-later
        (sc/value! prog-bar 0)))))
