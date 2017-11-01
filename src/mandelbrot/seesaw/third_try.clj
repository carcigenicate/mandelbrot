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
            [mandelbrot.coloring :as co]
            [mandelbrot.image-producer.producer :as mp])

  (:import [java.awt.event MouseEvent WindowEvent KeyEvent]
           [java.util Date]
           [java.awt Canvas]
           (javax.swing Timer)))

; TODO: Global frame!?

(def default-window-width 800)
(def default-window-ratio 1.5)
(def default-window-height (/ default-window-width default-window-ratio))

(def zoom-perc 0.90)

(def text-font "Arial-40-Bold")

(def chunk-perc (double 1/15))

(def default-save-width 5472)
(def save-width-ratio 2/3)

(def starting-limits
  (assoc l/full-map
    :rep-width default-window-width
    :rep-height default-window-height)) ; Awful workaround!

(def global-limits!
  (atom starting-limits))

(def global-finder-pair!
  (atom nil))

(def global-results
  (atom []))

(def color-options [co/lava, co/tentacles, co/exp, co/exp2, co/dull, co/crazy])

(def location-options [l/full-map, l/hand-of-god, l/swirl, l/center-spiral, l/tentacle-example])

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

(defn start-point-supplier [limits]
  (cf/point-calculator-component2 chunk-perc limits))

(defn start-receiving! [canvas chunk-chan]
  (go-loop []
    (when-let [chunk (<! chunk-chan)]
      (swap! global-results #(conj % chunk))
      (recur))))

(defn stop-current-process! []
  (swap! global-finder-pair!
         #(when-let [[_ stop-f] %]
           (stop-f)
           nil)))

(defn reset-finder-process!
  "Stops a running process (if ones already been started), and start a new one for the current limits."
  [cvs]
  (let [limits @global-limits!]
    #_
    (println "Starting finder for" limits)

    (stop-current-process!)
    (reset! global-results [])

    (reset! global-finder-pair!
            (let [[point-chan :as pair] (start-point-supplier limits)]
             (start-receiving! cvs point-chan)
             pair))))

(defn mouse-handler [canvas, ^MouseEvent e]
  (let [{:keys [start-r end-r, start-i end-i,
                rep-width rep-height]} @global-limits!
        left-click? (= MouseEvent/BUTTON1 (.getButton e))

        [x y :as s-coord]
        [(.getX e) (.getY e)]

        [r i :as comp-coord]
        (mapv double
              [(g/map-range x 0 rep-width start-r end-r)
               (g/map-range y 0 rep-height start-i end-i)])]

    (println "Complex:" comp-coord ", Screen:" s-coord)
    (swap! global-limits! #(-> %
                               (sh/move-limits-to r i)
                               (sh/zoom-limits-by-perc left-click? zoom-perc)))

    (reset-finder-process! canvas)))

(defn save-handler [root _]
  (println "Saving...")
  (let [prog-bar (sc/select root [:#save-progress])
        slider (sc/select root [:#save-width-slider])
        width (sc/value slider)
        height (* width save-width-ratio)]

    (future
      (mp/canvas-saver prog-bar @global-color-f!
                       (assoc @global-limits! :rep-width width,
                                              :rep-height height))
      (println "Saved.")
      (sc/invoke-later
        (sc/value! prog-bar 0)))))

(defn canvas []
  (let [cvs (sc/canvas :id :canvas
                       :paint (paint-map))]
    (sc/listen cvs
      :mouse-clicked (partial mouse-handler cvs))

    cvs))

(defn save-button [root]
  (sc/button :text "Save"
             :listen [:action (partial save-handler root)]))

(defn new-save-panel [root]
  (let [slider-label (sc/label :text (str default-save-width), :font text-font)
        width-slider (sc/slider :min 500, :max 10000, :id :save-width-slider)
        slider-panel (sc/horizontal-panel :items [width-slider slider-label])
        pb (sc/progress-bar :min 0, :max 100, :value 0
                            :id :save-progress)

        save-panel (sc/border-panel :north slider-panel
                                    :center (save-button root)
                                    :south pb)]

    ; For some reason specifying this in the slider constructor doesn't have any effect
    (sc/value! width-slider default-save-width)

    (sc/listen width-slider
       :change (fn [_] (sc/text! slider-label (str (sc/value width-slider)))))

    save-panel))

(defn new-movement-bar [cvs]
  (let [handler (fn [sym-code r-dir i-dir _]
                  (swap! global-limits!
                         (fn [l]
                           (let [[r-off i-off] (map #(/ % 2) (sh/limit-dimensions l))]
                             (sh/move-limits-by l (* r-dir r-off) (* i-dir i-off)))))

                  (reset-finder-process! cvs))

        b #(sc/button :text (str (char %)), :font text-font
                      :listen [:action (partial handler % %2 %3)])

        p (sc/flow-panel
            :items [(b 8593 0 -1) ; Up
                    (b 8595 0 1) ; Down
                    (b 8592 -1 0) ; Left
                    (b 8594 1 0)])] ; Right

    p))


(defn new-color-picker [cvs]
  (let [b #(sc/button :font text-font :text (str %)
                      :listen [:action
                               (fn [_] (reset! global-color-f! %2)
                                       (sc/repaint! cvs))])
        label (sc/label :font text-font, :text "Colors")
        buttons (map b (range) color-options)
        panel (sc/vertical-panel :items (conj buttons label))]

    panel))

(defn new-location-picker [root-frame]
  (let [cvs (sc/select root-frame [:#canvas])
        label (sc/label :font text-font, :text "Locations")
        b #(sc/button :font text-font, :text (str %)
                      :halign :center
                      :listen [:action
                               (fn [_]
                                 (let [[w h] (sh/get-dimensions root-frame)]
                                   (reset! global-limits!
                                           (assoc %2 :rep-width w
                                                     :rep-height h))
                                   (reset-finder-process! cvs)))])
        buttons (map b (range) location-options)
        panel (sc/vertical-panel :items (conj buttons label))]

    panel))

(defn panel []
  (let [cvs (canvas)

        bp (sc/border-panel
             :center cvs
             :north (new-movement-bar cvs)
             :east (new-color-picker cvs)
             :west (new-location-picker cvs))

        sp (new-save-panel bp)]

    (println "Loading...")
    (reset-finder-process! cvs)

    (sc/config! bp :south sp)

    bp))

(defn resize-checking-timer [root-frame check-delay]
  (let [cvs [(sc/select root-frame [:#canvas])]]
    (sc/timer
      (fn [_]
        (let [updated? (atom false)] ; Eww!
          (swap! global-limits!
                 #(let [{:keys [rep-width rep-height]} %
                        [w h] (sh/get-dimensions root-frame)]
                    (reset! updated? (or (not= w rep-width) (not= h rep-height)))
                    (assoc % :rep-width w, :rep-height h)))

          (when @updated?
            (reset-finder-process! cvs))))

      :delay check-delay)))

(defn stop-timers [& timers]
  (doseq [t timers]
    (.stop ^Timer t)))

(defn frame []
  (let [f (sc/frame :size [default-window-width :by default-window-height]
                    :content (panel))

        cvs (sc/select f [:#canvas])

        resize-timer (resize-checking-timer f 2500)

        repaint-timer (sc/timer (fn [_] (sc/repaint! cvs))
                                :delay 1000)]

    (sc/listen f
       :window-closing
       (fn [_] (stop-current-process!)
               (stop-timers resize-timer repaint-timer)))

    (sc/request-focus! f)

    f))