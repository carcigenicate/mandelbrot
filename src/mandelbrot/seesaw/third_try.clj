(ns mandelbrot.seesaw.third-try
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [seesaw.color :as s-col]
            [seesaw.graphics :as sg]

            [helpers.general-helpers :as g]

            [clojure.core.async :refer [go go-loop <! >! chan thread]]

            [mandelbrot.seesaw.color-picker :as cp]
            [mandelbrot.mandelbrot-iteration :as mi]
            [mandelbrot.locations :as l]
            [mandelbrot.concurrent-finder :as cf]
            [mandelbrot.seesaw.helpers :as sh]
            [mandelbrot.coloring :as co]
            [mandelbrot.image-producer.producer :as mp])

  (:import [java.awt.event MouseEvent WindowEvent KeyEvent]
           [java.util Date]
           [java.awt Canvas Color]
           [javax.swing Timer JPanel JComponent]))

; TODO: Global frame!?

(def default-window-width 900)
(def default-window-ratio 0.7)
(def default-window-height (* default-window-width default-window-ratio))

(def zoom-perc 0.9)
(def move-perc 0.4)

(def text-font "Arial-16")
(def stat-font "Arial-15")

(def default-save-width 5472)
(def save-width-ratio 2/3)

(def chunk-perc (delay (double (/ 1 (* (sh/available-processors) 2)))))

(def default-starting-limits
  (assoc l/full-map
    :rep-width default-window-width
    :rep-height default-window-height)) ; Awful workaround!

(def global-limits!
  (atom default-starting-limits))

(def global-finder-pair!
  (atom nil))

(def global-results
  (atom []))

(def color-options [co/lava, co/tentacles, co/exp, co/exp2,
                    co/dull, co/crazy, co/super-crazy, co/quad])

(def location-options [l/full-map, l/hand-of-god, l/swirl, l/center-spiral,
                       l/tentacle-example, l/evolving-swirls])

(def global-color-f! (atom co/exp))

(defn paint [c g]
  (let [chunks @global-results
        color-f @global-color-f!]
    (doseq [chunk chunks]
      (doseq [{:keys [r i, rep-x rep-y, iters]} chunk]
        (sg/draw g
           (sg/rect rep-x rep-y 1 1)
           (sg/style :background (color-f r i iters)))))))

(defn paint-map []
  {:after paint, :super? true})

(defn start-point-supplier
  "Starts the finder for the given limits, and returns the [result-chan stop-f] pair."
  [limits]
  (cf/point-calculator-component @chunk-perc limits))

(defn start-receiving!
  "Loops while the given chunk channel is open and supplying chunks.
  Repaints every few seconds, and when the process is completed."
  [canvas chunk-chan]
  (let [repaint-delay 2500
        t (sc/timer (fn [_] (sc/repaint! canvas))
                    :initial-delay repaint-delay
                    :delay repaint-delay)]
    (go-loop []
      (if-let [chunk (<! chunk-chan)]
        (do
          (swap! global-results #(conj % chunk))
          (recur))

        (do
          (.stop t)
          (sc/repaint! canvas))))))

(defn stop-current-process!
  "Cancels the global finder."
  []
  (swap! global-finder-pair!
         #(when-let [[_ stop-f] %]
           (stop-f)
           nil)))

(defn reset-finder-process!
  "Stops a running process (if ones already been started), and starts a new one for the current limits."
  [cvs]
  (let [{:keys [rep-width rep-height] :as limits} @global-limits!]
    (when (pos? (* rep-width rep-height))
      (stop-current-process!)
      (reset! global-results [])

      (reset! global-finder-pair!
              (let [[point-chan :as pair] (start-point-supplier limits)]
               (start-receiving! cvs point-chan)
               pair)))))

(defn mouse-handler [^JComponent canvas, ^MouseEvent e]
  (let [{:keys [start-r end-r, start-i end-i,
                rep-width rep-height]} @global-limits!
        g (.getGraphics canvas)
        left-click? (= MouseEvent/BUTTON1 (.getButton e))

        [x y] [(.getX e) (.getY e)]

        [r i]
        (mapv double
              [(g/map-range x 0 rep-width start-r end-r)
               (g/map-range y 0 rep-height start-i end-i)])

        min-dim (min rep-width rep-height)
        dot-radius (/ (* (- 1 zoom-perc) min-dim) 2)]

    (sg/draw g
             (sg/circle x y dot-radius)
             (sg/style :background (Color. 255 255 255 150)))

    (swap! global-limits! #(-> %
                               (sh/move-limits-to r i)
                               (sh/zoom-limits-by-perc left-click? zoom-perc)))

    (reset-finder-process! canvas)))

(defn save-handler [root _]
  (println (str "Saving... (" (Date.) ")"))
  (let [prog-bar (sc/select root [:#save-progress])
        time-label (sc/select root [:#time-remaining])
        slider (sc/select root [:#save-width-slider])
        width (sc/value slider)
        height (* width save-width-ratio)]

    (thread
      (try
        (mp/canvas-saver prog-bar time-label
                         @global-color-f!
                         (assoc @global-limits! :rep-width width,
                                                :rep-height height))
        (println "Saved at" (str (Date.)))

        (catch OutOfMemoryError e
          (println (str "Sorry! That image is too big to be created on this computer!")))

        (finally
          (sc/invoke-later
            (sc/value! prog-bar 0)
            (sc/text! time-label "")))))))


(defn canvas []
  (let [cvs (sc/canvas :id :canvas
                       :paint (paint-map))]
    (sc/listen cvs
      :mouse-released (partial mouse-handler cvs))

    cvs))

(defn save-button [root]
  (sc/button :text "Save", :font text-font
             :listen [:action (partial save-handler root)]))

(defn new-save-panel []
  (let [slider-label (sc/label :text (str default-save-width), :font text-font)
        time-label (sc/label :font text-font, :id :time-remaining)
        width-slider (sc/slider :min 100, :max 50000, :id :save-width-slider)
        slider-panel (sc/horizontal-panel :items [slider-label width-slider time-label])
        pb (sc/progress-bar :min 0, :max 100, :value 0
                            :paint-string? true, :id :save-progress)

        save-panel (sc/border-panel :north slider-panel
                                    :south pb)

        save-button (save-button save-panel)]

    (sc/config! save-panel :center save-button)

    ; For some reason specifying this in the slider constructor doesn't have any effect
    (sc/value! width-slider default-save-width)

    (sc/listen width-slider
       :change (fn [_] (sc/text! slider-label (str (sc/value width-slider)))))

    save-panel))

(defn update-location-stat! [root]
  (let [loc-stat (sc/select root [:#location-stat])
        {:keys [start-r end-r, start-i end-i]} @global-limits!]
    (sc/text! loc-stat
              (str "<html>start-r: " start-r ", end-r: " end-r "<br>"
                   "start-i: " start-i ", end-i: " end-i "</html>"))))

(defn update-stat-bar! [root]
  (doto root
    (update-location-stat!)))

(defn new-stat-bar []
  (let [location-stat (sc/label :font stat-font, :id :location-stat)
        panel (sc/flow-panel :items [location-stat])]

    (update-stat-bar! panel)

    panel))

(defn new-movement-bar [cvs]
  (let [handler (fn [sym-code r-dir i-dir _]
                  (swap! global-limits!
                         (fn [l]
                           (let [[r-off i-off] (map #(* % move-perc)
                                                    (sh/plane-dimensions l))]
                             (sh/move-limits-by l (* r-dir r-off)
                                                  (* i-dir i-off)))))

                  (reset-finder-process! cvs))

        b #(sc/button :text (str (char %)), :font text-font
                      :listen [:action (partial handler % %2 %3)])

        p (sc/flow-panel
            :items [(b 8593 0 -1) ; Up
                    (b 8595 0 1) ; Down
                    (b 8592 -1 0) ; Left
                    (b 8594 1 0)])] ; Right

    p))

(defn new-color-preset-panel [cvs]
  (let [b #(sc/button :font text-font :text (str %)
                      :listen [:action
                               (fn [_] (reset! global-color-f! %2)
                                       (sc/repaint! cvs))])
        label (sc/label :font text-font, :text "Colors")
        buttons (map b (range) color-options)
        panel (sc/vertical-panel :items (conj buttons label))]

    panel))

(defn new-color-panel [cvs]
  (let [presets (new-color-preset-panel cvs)
        picker (cp/new-option-panel cvs global-color-f!)

        bp (sc/border-panel :west presets,
                            :center picker)]
    bp))

(defn new-location-picker [root-frame]
  (let [cvs (sc/select root-frame [:#canvas])
        label (sc/label :font text-font, :text "Locations")
        b #(sc/button :font text-font, :text (str %)
                      :halign :center
                      :listen [:action
                               (fn [_] ; FIXME: HERE!
                                 (let [[w h] (sh/get-dimensions cvs)]
                                   (reset! global-limits!
                                           (assoc %2 :rep-width w
                                                     :rep-height h))
                                   (reset-finder-process! cvs)))])

        buttons (map b (range) location-options)
        panel (sc/vertical-panel :items (conj buttons label))]

    panel))

(defn new-view-panel []
  (let [cvs (canvas)

        bp (sc/border-panel
             :center cvs
             :north (new-movement-bar cvs)
             :west (new-location-picker cvs))

        save-panel (new-save-panel)
        stat-panel (new-stat-bar)
        south-panel (sc/vertical-panel :items [save-panel stat-panel])]

    (sc/config! bp :south south-panel)

    (reset-finder-process! cvs)

    bp))

(defn main-panel []
  (let [view-panel (new-view-panel)
        cvs (sc/select view-panel [:#canvas])

        color-panel (new-color-panel cvs)

        bp (sc/border-panel :center view-panel
                            :east color-panel)]

    bp))

(defn updating-timer
  "Checks for resizes. If the window has been resized since the last check, the finder is reset for the new canvas size."
  [root-frame check-delay]
  (let [cvs (sc/select root-frame [:#canvas])
        timer-f
            (fn [_]
              (let [updated? (atom false)] ; Eww!
                (swap! global-limits!
                       #(let [{:keys [rep-width rep-height]} %
                              [w h] (sh/get-dimensions cvs)]
                          (reset! updated? (or (not= w rep-width)
                                               (not= h rep-height)))
                          (assoc % :rep-width w, :rep-height h)))

                (when @updated?
                  (reset-finder-process! cvs))

                (update-stat-bar! root-frame)))]

    (sc/timer timer-f :initial-delay check-delay, :delay check-delay)))

(defn stop-timers [& timers]
  (doseq [t timers]
    (.stop ^Timer t)))

(defn frame [& [start-r end-r, start-i end-i]]
  (reset! global-limits! (if start-r
                           (cf/->Mandelbrot-Limits start-r end-r,
                                                   start-i end-i
                                                   default-window-width
                                                   default-window-height)
                           default-starting-limits))

  (let [f (sc/frame :size [default-window-width :by default-window-height]
                    :content (main-panel))

        cvs (sc/select f [:#canvas])

        update-timer (updating-timer f 2500)]

    (sc/listen f
       :window-closing
       (fn [_] (stop-current-process!)
               (stop-timers update-timer)
               (println "Final Cleanup Performed.")))

    (sc/request-focus! f)

    f))