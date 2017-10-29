(ns mandelbrot.seesaw.third-try
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [seesaw.color :as s-col]
            [seesaw.graphics :as sg]

            [helpers.general-helpers :as g]

            [clojure.core.async :refer [go go-loop <! >! chan]]

            [mandelbrot.mandelbrot-iteration :as mi]
            [mandelbrot.locations :as l]
            [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.seesaw.helpers :as sh]
            [mandelbrot.coloring :as co])

  (:import [java.awt.event MouseEvent WindowEvent KeyEvent]
           [java.util Date]))

(def window-width 700)
(def window-ratio 1)
(def window-height (/ window-width window-ratio))

(def zoom-perc 0.90)

(def chunk-perc (double 1/10))

(def standard-limits
  (assoc l/swirl
    :rep-width window-width
    :rep-height window-height) ; Awful workaround!
  #_
  (cf/->Mandelbrot-Limits
     0 0.5, 0 0.5
     window-width window-height))

(def global-limits
  (atom standard-limits))

(def global-finder-pair
  (atom nil))

(def global-results
  (atom []))

(def color-f co/exp)

#_
(defn color-f [x y iters]
  (let [w #(g/wrap % 0 255)
        c #(g/clamp % 0 255)
        w-i (w iters)
        fact 0.01]

    (s-col/color (c (* iters iters iters fact x)) (c (* iters iters fact y)) (c (* iters fact)))))

(defn paint [c g]
  (let [chunks @global-results]
    (doseq [chunk chunks]
      (doseq [{:keys [r i, rep-x rep-y, iters]} chunk]
        (sg/draw g
           (sg/rect rep-x rep-y 1 1)
           (sg/style :background (color-f r i iters)))))))

(defn paint-map []
  {:after paint, :super? true})

(defn start-point-supplier [limits]
  (cf/point-calculator-component2 chunk-perc limits))

(defn start-receiving! [canvas chunk-chan]
  (go-loop []
    (when-let [chunk (<! chunk-chan)]
      (swap! global-results #(conj % chunk))
      (recur))))

(defn stop-current-process! []
  (swap! global-finder-pair
    #(when-let [[_ stop-f] %]
       (stop-f)
       nil)))

(defn reset-finder-process!
  "Stops a running process (if ones already been started), and start a new one for the current limits."
  [cvs]
  (let [limits @global-limits]
    #_
    (println "Starting finder for" limits)

    (stop-current-process!)
    (reset! global-results [])

    (reset! global-finder-pair
      (let [[point-chan :as pair] (start-point-supplier limits)]
        (start-receiving! cvs point-chan)
        pair))))

(defn mouse-handler [canvas, ^MouseEvent e]
  (let [{:keys [start-r end-r, start-i end-i,
                rep-width rep-height]} @global-limits
        left-click? (= MouseEvent/BUTTON1 (.getButton e))

        [x y :as s-coord]
        [(.getX e) (.getY e)]

        [r i :as comp-coord]
        (mapv double
              [(g/map-range x 0 rep-width start-r end-r)
               (g/map-range y 0 rep-height start-i end-i)])]

    (println "Complex:" comp-coord ", Screen:" s-coord)
    (swap! global-limits #(-> %
                              (sh/move-limits-to r i)
                              (sh/zoom-limits-by-perc left-click? zoom-perc)))

    (reset-finder-process! canvas)))

(defn save-handler [canvas _]
  (println "Saving...")

  (sh/save-snapshot @global-limits
    (sh/get-snapshot canvas)))

(defn canvas []
  (let [cvs (sc/canvas :id :canvas
                       :paint (paint-map))]
    (sc/listen cvs
      :mouse-clicked (partial mouse-handler cvs))

    cvs))

(defn save-button [canvas]
  (sc/button :text "Save"
             :listen [:action (partial save-handler canvas)]))

(defn panel []
  (let [cvs (canvas)
        save-btn (sc/button :text "Save"
                            :listen [:action (partial save-handler cvs)])
        bp (sc/border-panel
             :center cvs
             :south (sc/flow-panel :items [save-btn]
                                   :align :center))]


    (println "Loading...")
    (reset-finder-process! cvs)

    (sc/timer
      (fn [_] (sc/repaint! cvs))
      :delay 500)

    bp))

(defn frame []
  (let [f (sc/frame :size [window-width :by window-height]
                    :content (panel)
                    :resizable? false)]

    (sc/listen f
       :window-closing
       (fn [_] (stop-current-process!)))

    f))