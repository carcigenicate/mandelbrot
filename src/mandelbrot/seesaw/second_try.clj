(ns mandelbrot.seesaw.second-try
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [seesaw.color :as s-col]
            [seesaw.graphics :as sg]

            [helpers.general-helpers :as g]

            [clojure.core.async :refer [go go-loop <! >! chan]]

            [mandelbrot.mandelbrot-iteration :as mi]
            [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.seesaw.helpers :as sh])

  (:import [java.awt.event MouseEvent]
           [java.util Date]))

(def window-width 600)
(def window-ratio 1)
(def window-height (/ window-width window-ratio))

; FIXME: NEED A WAY TO STOP PROCESSING OF BACKGROUND CALCULATIONS.
; FIXME: CHANGE FROM DOSEQ TO EXPLICIT LOOP, AND CONTROL VIA A FLAG?

; FIXME: Will probably need to be a function of the current zoom level
; How to get the current zoom level from limits?
(def zoom-perc 0.9999999)

(def chunk-perc (double 1/20))

(def standard-limits
  (cf/->Mandelbrot-Limits
     0 0.5, 0 0.5
     window-width window-height))

(def global-limits
  (atom standard-limits))

(defn color-f [x y iters]
  (let [w #(g/wrap % 0 255)
        c #(g/clamp % 0 255)
        w-i (w iters)]

    (s-col/color (c (* x iters 10)) (c (* y iters 5)) (w (* iters 2)))))

(defn paint [chnk c g]
  (println "Drawing chunk..." (str (Date.)))

  (doseq [{:keys [r i, rep-x rep-y, iters]} chnk]
    (sg/draw g
             (sg/rect rep-x rep-y 1 1)
             (sg/style :background (color-f r i iters)))))

(defn paint-map [chnk]
  {:after (partial paint chnk), :super? false})

(defn start-point-supplier []
  (cf/generated-find-point-chan chunk-perc @global-limits))

(defn start-receiving [canvas chunk-chan]
  (go-loop []
    (when-let [chunk (<! chunk-chan)]
      ; Blocking. Need to make sure we aren't flooding paint requests while one is still running
      (sc/invoke-now
        (sc/config! canvas :paint (paint-map chunk))
        (sc/repaint! canvas))
      (recur))))

(defn start-finder-process [cvs]
  (println "Starting finder for" @global-limits)
  ; So we can have a "recalc" button.
  ; TODO: Should access a record of the tweaks given to the async finders
  (let [point-chan (start-point-supplier)]
    (start-receiving cvs point-chan)))

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

    (start-finder-process canvas)
    (sc/config! canvas :paint (constantly nil))))

(defn canvas []
  (let [cvs (sc/canvas :id :canvas)]
    (sc/listen cvs :mouse-clicked (partial mouse-handler cvs))
    cvs))

(defn panel []
  (let [cvs (canvas)
        reset-btn (sc/button)
        bp (sc/border-panel :center cvs)]

    (println "Loading...")
    (time (start-finder-process cvs))

    bp))

(defn frame []
  (let [f (sc/frame :size [window-width :by window-height]
                    :content (panel)
                    :resizable? false)]
    f))