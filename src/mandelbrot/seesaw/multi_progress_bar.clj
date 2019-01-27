(ns mandelbrot.seesaw.multi-progress-bar
  (:require [seesaw.core :as sc]

            [mandelbrot.seesaw.helpers :as sh]
            [seesaw.graphics :as sg]))

(def prog-text-height-perc 0.3)
(def prog-text-x-offset-perc 0.01)

(def line-width 10)

; TODO: Allow line width, color, etc to be set
; TODO: Handle bad IDs better. Currently causes an NPE.

(defn clamp-n [n min-n max-n]
  (-> n
      (max min-n)
      (min max-n)))

(defn new-progress-state [max-progress]
  {:current-progress 0, :max-progress max-progress})

(defn paint-progress [progress-state-atom canvas g]
  (let [state-map @progress-state-atom

        [cw ch] (sh/component-dimensions canvas)

        font-size (int (* ch prog-text-height-perc))]

    (doseq [[id {:keys [current-progress max-progress]}] state-map

            :let [prog-perc (double (/ current-progress max-progress))
                  x (* cw prog-perc)

                  text-x (+ x (* cw prog-text-x-offset-perc))
                  text-y (+ font-size
                            (rem (* id font-size) (- ch font-size)))
                  desc-str (str id ": " (sh/format-to-n-places (* prog-perc 100) 1) " %")]]

      (sg/draw g
         (sg/line x 0, x ch)
         (sg/style :foreground :black, :stroke line-width)

         (sg/string-shape text-x text-y desc-str)
         (sg/style :font {:size font-size})))))

(defn new-multi-progress-bar [& seesaw-args]
  (let [progress-state-atom (atom {})

        canvas (sc/canvas :paint (partial paint-progress progress-state-atom))

        prog-bar (apply sc/horizontal-panel :items [canvas] seesaw-args)]

    (sc/config! prog-bar :user-data progress-state-atom)

    prog-bar))

(defn- update-state! [prog-bar f & args]
  (let [state-atom (sc/user-data prog-bar)]
    (apply swap! state-atom f args)))

(defn add-job! [prog-bar id max-progress]
  (update-state! prog-bar
    assoc id (new-progress-state max-progress)))

(defn remove-job! [prog-bar id]
  (update-state! prog-bar
    dissoc id))

(defn set-progress! [prog-bar id new-progress]
  (update-state! prog-bar
    #(assoc-in % [id :current-progress]
               (clamp-n new-progress 0 (get-in % [id :max-progress])))))

(defn increment-progress! [prog-bar id progress-to-add]
  (update-state! prog-bar
    #(update-in % [id :current-progress]
       (fn [p] (clamp-n (+ p progress-to-add) 0 (get-in % [id :max-progress]))))))

(defn get-progress-snapshot [prog-bar]
  (let [stat-atom (sc/user-data prog-bar)]
    @stat-atom))

(defn new-test-frame []
  (let [bar (new-multi-progress-bar)

        frame (sc/frame :content bar, :size [1000 :by 200])]

    (doto bar
      (add-job! 1234567 100)
      (set-progress! 1234567 33))

    (sc/repaint! frame)

    frame))
