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

(def default-window-width 900)
(def default-window-ratio 0.7)
(def default-window-height (* default-window-width default-window-ratio))

(def move-perc 0.30)

(def min-zoom-perc -1.0)
(def max-zoom-perc 2.0)
(def default-zoom-perc 0.9)

(def text-font "Arial-16")
(def stat-font "Arial-15")

(def default-save-width 300)
(def save-width-ratio 2/3)

(def shutdown-delay 30)

(def repaint-delay 1000)

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

(def color-options [co/lava, co/tentacles, co/exp, co/exp2, co/dull, co/crazy, co/super-crazy, co/quad, co/range-coloring, co/grey-scale])

(def location-options [l/full-map, l/hand-of-god, l/swirl, l/center-spiral,
                       l/tentacle-example, l/evolving-swirls l/tool-swirls])

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
  (let [t (sc/timer (fn [_] (sc/repaint! canvas))
                    :initial-delay repaint-delay
                    :delay repaint-delay)]
    (go-loop []
      (if-let [chunk (<! chunk-chan)]
        (do
          (swap! global-results #(conj % chunk))
          (recur))

        (do
          (.stop ^Timer t)
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

(defn update-location-stat! [root]
  (let [loc-stats (sc/select root [:.location-entry])
        {:keys [start-r end-r, start-i end-i]} @global-limits!]
    ; FIXME: EWWWWW!
    (doseq [[com stat] (map vector loc-stats [start-r end-r start-i end-i])]
      (sc/text! com stat))))

(defn update-stat-bar! [root]
  (doto root
    (update-location-stat!)))

(defn mouse-handler [root, ^MouseEvent e]
  (let [cvs (sc/select root [:#canvas])
        zoom-perc (sc/selection (sc/select root [:#zoom-perc-input]))

        {:keys [start-r end-r, start-i end-i,
                rep-width rep-height]} @global-limits!
        g (.getGraphics ^JComponent cvs)
        left-click? (= MouseEvent/BUTTON1 (.getButton e))

        [x y] [(.getX e) (.getY e)]

        [r i]
        (mapv double
              [(g/map-range x 0 rep-width start-r end-r)
               (g/map-range y 0 rep-height start-i end-i)])

        avg-dim (/ (+ rep-width rep-height) 2)
        dot-radius (/ (* (- 1 zoom-perc) avg-dim) 2)]

    (sg/draw g
             (sg/circle x y dot-radius)
             (sg/style :background (Color. 255 255 255 175)))

    (swap! global-limits! #(-> %
                               (sh/move-limits-to r i)
                               (sh/zoom-limits-by-perc left-click? zoom-perc)))

    (reset-finder-process! cvs)
    (update-stat-bar! root)))

(defn delayed-shutdown [& [s-delay?]]
  (let [t-str (if s-delay? (str " -t " s-delay?) "")]
    (.exec (Runtime/getRuntime)
           (str "shutdown -s" t-str))))

(defn save-handler [save-root _]
  (println (str "Saving... (" (Date.) ")"))
  (let [prog-bar (sc/select save-root [:#save-progress])
        time-label (sc/select save-root [:#time-remaining])
        width-input (sc/select save-root [:#save-width-input])

        close-on-save? #(sc/config (sc/select save-root
                                              [:#close-on-save?-checkbox])
                                   :selected?)

        width (sc/value width-input)
        height (* width save-width-ratio)]

    (thread
      (try
        (mp/canvas-saver prog-bar time-label
                         @global-color-f!
                         (assoc @global-limits! :rep-width width,
                                                :rep-height height))
        (println "Saved at" (str (Date.) "\n"))

        (catch OutOfMemoryError e
          (println (str "Sorry! That image is too big to be created on this computer! Try increasing the allocated heap size.")))

        (finally
          (when (close-on-save?)
            (delayed-shutdown shutdown-delay)
            (System/exit 0))

          (sc/invoke-later
            (sc/value! prog-bar 0)
            (sc/text! time-label "")))))))

(defn new-canvas []
  (let [cvs (sc/canvas :id :canvas
                       :paint (paint-map))]

    cvs))

(defn teleport-to [target-limits canvas]
  (let [[w h] (sh/get-dimensions canvas)]
    (reset! global-limits!
            (assoc target-limits
              :rep-width w
              :rep-height h))

    (reset-finder-process! canvas)))

(defn save-button [save-root]
  (sc/button :text "Save", :font text-font
             :listen [:action (partial save-handler save-root)]))

(defn new-save-panel []
  (let [time-label (sc/label :font text-font, :id :time-remaining)
        width-label (sc/label :font text-font, :text "Saved Image Width: ")
        width-input (sc/spinner :model default-save-width, :font text-font,
                                :id :save-width-input,
                                :tip "A number between 1000 and 40000")
        width-panel (sc/horizontal-panel :items [width-label width-input time-label])
        pb (sc/progress-bar :min 0, :max 100, :value 0
                            :paint-string? true, :id :save-progress)

        save-opts (sc/flow-panel
                    :items [(sc/label :text "Close and shutdown computer when done saving?"
                                      :font text-font)
                            (sc/checkbox :id :close-on-save?-checkbox)])

        save-panel (sc/vertical-panel :items [width-panel
                                              pb
                                              save-opts]
                                      :border 5)

        save-button (save-button save-panel)]

    (sc/add! save-panel save-button)

    save-panel))

(defn new-teleport-button [canvas location-entries]
  (let [to-limits #(apply cf/repless-limits %)
        handler (fn [_] (let [coords? (map (comp g/parse-double sc/text) location-entries)]
                          ; FIXME: Is failing to activate sometimes.
                          ; TODO: Add trimming
                          (when (every? identity coords?)
                            (teleport-to (to-limits coords?) canvas))))

        tele-btn (sc/button :text "Teleport to...", :font stat-font
                            :listen [:action handler])]

    tele-btn))

(defn new-stat-bar [canvas]
  (let [e #(sc/text :font stat-font, :class :location-entry, :halign :center, :columns 18)
        es (for [_ (range 4)] (e))
        grid (sc/grid-panel :columns 2, :items es)
        tele-btn (new-teleport-button canvas es)

        stat-bar (sc/flow-panel :items [tele-btn grid])]

    (update-stat-bar! stat-bar)

    stat-bar))

(defn new-zoom-panel []
  (let [zoom-label (sc/label :text "Zoom Percentage: ", :font text-font)
        zoom-input (sc/spinner
                     :model
                     (sc/spinner-model default-zoom-perc
                        :from min-zoom-perc, :to max-zoom-perc, :by 0.01)
                     :font text-font, :id :zoom-perc-input
                     :tip "A decimal percentage of the screen to zoom by each click.")]

    (sc/horizontal-panel :items [zoom-label zoom-input])))

(defn new-movement-bar [root]
  (let [cvs (sc/select root [:#canvas])
        handler (fn [r-dir i-dir _]
                  (swap! global-limits!
                         (fn [l]
                           (let [[r-off i-off] (map #(* % move-perc)
                                                    (sh/plane-dimensions l))]
                             (sh/move-limits-by l (* r-dir r-off)
                                                  (* i-dir i-off)))))

                  (reset-finder-process! cvs)
                  (update-stat-bar! root))

        b #(sc/button :text (str (char %)), :font text-font
                      :listen [:action (partial handler %2 %3)])

        pan-panel (sc/flow-panel
                    :items [(b 8593 0 -1) ; Up
                            (b 8595 0 1) ; Down
                            (b 8592 -1 0) ; Left
                            (b 8594 1 0)]) ; Right

        zoom-panel (new-zoom-panel)

        movement-panel (sc/vertical-panel :items [pan-panel zoom-panel])]

    movement-panel))

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
                               (fn [_] (teleport-to %2 cvs)
                                 (update-stat-bar! root-frame))])


        buttons (map b (range) location-options)
        panel (sc/vertical-panel :items (conj buttons label))]

    panel))

(defn new-view-panel []
  (let [cvs (new-canvas)

        bp (sc/border-panel :center cvs)

        save-panel (new-save-panel)
        stat-panel (new-stat-bar cvs)
        south-panel (sc/vertical-panel :items [stat-panel save-panel])]

    (sc/config! bp :north (new-movement-bar bp)
                   :west (new-location-picker bp)
                   :south south-panel)

    (sc/listen cvs :mouse-released (partial mouse-handler bp))

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
                  (reset-finder-process! cvs))))]

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