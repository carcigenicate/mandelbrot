(ns seesaw-tests.canvas-tests
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [seesaw.color :as s-col]
            [seesaw.graphics :as sg]))

(def def-size 200)

(defn paint [r c g]
  (let [c (rem r 255)]
    (sg/draw g
      (sg/circle r r r)
      (sg/style :background (s-col/color c c c)))))

(defn paint-map [r]
  {:after (partial paint r), :super? false})

(defn canvas []
  (sc/canvas :paint (partial paint def-size)))

(defn panel []
  (let [canv (canvas)

        slider (sc/slider
                 :min 10, :max 1000, :value def-size)

        bp (sc/border-panel :center canv
                            :south slider)]

    (sc/listen slider
       :change
       (fn [_]
         (sc/config! canv :paint
           (paint-map (sc/value slider)))))

    bp))

(defn frame []
  (let [f (sc/frame :size [1000 :by 1000]
                    :content (panel))]
    f))