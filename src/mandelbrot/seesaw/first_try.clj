(ns mandelbrot.seesaw.first-try
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [seesaw.color :as s-col]
            [seesaw.graphics :as sg]

            [helpers.general-helpers :as g]

            [mandelbrot.mandelbrot-iteration :as mi]
            [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.seesaw.helpers :as sh]))

(def window-size 1200)

(defn test-matrix [side-length]
  (cf/future-rows2
    -1.5 1.5, -1.5 1.5
    side-length side-length))

(defn color-f [x y iters]
  (let [w #(g/wrap % 0 255)
        c #(g/clamp % 0 255)
        w-i (w iters)]

    (s-col/color w-i (c (* iters 0.05 x)) (c (* iters 0.1 y)))))

(defn paint [matrix c g]
  (println "Drawing...")
  (time
    (doseq [fut-row matrix]
      (doseq [{:keys [rep-x rep-y, iters]} @fut-row]

        (sg/draw g
           (sg/rect rep-x rep-y 1 1)
           (sg/style :background (color-f rep-x rep-y iters)))))))

(defn paint-map [matrix]
  {:after (partial paint matrix, :super? false)})

(defn canvas [starting-matrix]
  (sc/canvas :paint (partial paint starting-matrix)))

(defn panel []
  (let [canv (canvas (time (test-matrix window-size)))
        bp (sc/border-panel :center canv)]

    bp))

(defn frame []
  (let [f (sc/frame :size [window-size :by window-size]
                    :content (panel))]
    f))