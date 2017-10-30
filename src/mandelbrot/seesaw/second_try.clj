(ns mandelbrot.seesaw.second-try
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [seesaw.color :as s-col]
            [seesaw.graphics :as sg]

            [helpers.general-helpers :as g]

            [clojure.core.async :refer [go go-loop <! >! chan]]

            [mandelbrot.mandelbrot-iteration :as mi]
            [mandelbrot.locations :as l]
            [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.seesaw.helpers :as sh])

  (:import [java.awt.event MouseEvent WindowEvent KeyEvent]
           [java.util Date]))

(def window-width 700)
(def window-ratio 1.5)
(def window-height (/ window-width window-ratio))

; FIXME: NEED A WAY TO STOP PROCESSING OF BACKGROUND CALCULATIONS.
; FIXME: CHANGE FROM DOSEQ TO EXPLICIT LOOP, AND CONTROL VIA A FLAG?

(def zoom-perc 0.99999999999)

(def chunk-perc (double 1/5))

(def standard-limits
  (assoc l/center-spiral
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

(defn color-f [x y iters]
  (let [w #(g/wrap % 0 255)
        c #(g/clamp % 0 255)
        w-i (w iters)]

    (s-col/color (c (* x iters 10)) (c (* y iters 5)) (w (* iters 2)))))

(defn paint [chnk c g]
  (doseq [{:keys [r i, rep-x rep-y, iters]} chnk]
    (sg/draw g
             (sg/rect rep-x rep-y 1 1)
             (sg/style :background (color-f r i iters)))))

(defn paint-map [chnk]
  {:after (partial paint chnk), :super? false})

(defn start-point-supplier [limits]
  (cf/point-calculator-component chunk-perc limits)) ; TODO: Test!

(defn start-receiving! [canvas chunk-chan]
  (go-loop []
    (when-let [chunk (<! chunk-chan)]
      ; Blocking. Need to make sure we aren't flooding paint requests while one is still running
      (sc/invoke-now
        (sc/config! canvas :paint (paint-map chunk))
        (sc/repaint! canvas))
      (recur))))

(defn stop-current-process! []
  (println "Stopping...")
  (swap! global-finder-pair
    #(when-let [[_ stop-f] %]
       (stop-f)
       nil)))

(defn reset-finder-process!
  "Stops a running process (if ones already been started), and start a new one for the current limits."
  [cvs]
  (let [limits @global-limits]
    (println "Starting finder for" limits)

    (stop-current-process!)

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

    (reset-finder-process! canvas)
    (sc/config! canvas :paint (constantly nil))))

(defn canvas []
  (let [cvs (sc/canvas :id :canvas)]
    (sc/listen cvs
      :mouse-clicked (partial mouse-handler cvs))

    cvs))

(defn panel []
  (let [cvs (canvas)
        bp (sc/border-panel
             :center cvs)]

    (println "Loading...")
    (time (reset-finder-process! cvs))

    bp))

(defn frame []
  (let [f (sc/frame :size [window-width :by window-height]
                    :content (panel)
                    :resizable? false)]

    (sc/listen f
       :window-closing
       (fn [_] (stop-current-process!)))

    f))