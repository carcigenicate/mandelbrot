(ns mandelbrot.seesaw.color-picker
  (:require [seesaw.core :as sc]
            [seesaw.font :as sf]

            [mandelbrot.color-options :as co]

            [helpers.general-helpers :as g])

  (:import [java.awt Color]))

(def starting-mult 3)

(def default-max-rand-mag 1.5)

(def option-font (sf/font :size 15,
                          :name "Arial"))

(def global-rand-gen (g/new-rand-gen))

(defn all? [coll]
  (every? identity coll))

(defn affect-all-props [root class-selector prop-selector f]
  (doseq [wid (sc/select root [class-selector])]
    (sc/config! wid prop-selector
                (f (sc/config wid prop-selector)))))

(defn new-color-mult-panel [label]
  (let [l #(sc/label :text (str %), :font option-font
                     :border 0)

        t #(sc/text :text (str starting-mult), :font option-font,
                    :columns 4, :halign :center, :class :mult-input)

        lock #(sc/checkbox :selected? false, :class :lock-check)

        row #(sc/flow-panel :items [(l %) (t) (lock)], :align :center, :class :mult-row)

        main-options (sc/vertical-panel :items [(l label) (row "r") (row "i") (row "n")
                                                (sc/separator)]
                                        :class :mult-panel)

        #_(other-options (effect-all-options main-options :.mult-input))]

    main-options))

(defn mults-from-panel? [color-mult-panel]
  (let [sliders (sc/select color-mult-panel [:.mult-input])
        values? (map (comp g/parse-double sc/text) sliders)]

    (when (all? values?)
      (apply co/->Color-Mults values?))))

(defn options-from-panel [option-panel]
  (let [panels (sc/select option-panel [:.mult-panel])
        mults? (map mults-from-panel? panels)]

    (when (all? mults?)
      (apply co/->Color-Options mults?))))

(defn update-coloring! [root color-atom canvas]
  (when-let [opts? (options-from-panel root)]
    (reset! color-atom
            (co/new-basic-color-f opts?))

    (sc/invoke-later
      (sc/repaint! canvas))))

(defn new-update-button [canvas color-atom option-panel]
  (let [handler (fn [_]
                  (update-coloring! option-panel color-atom canvas))]

    (sc/button :text "Update",
               :font option-font
               :listen [:action handler])))

(defn random-button-handler [root canvas color-atom _]
  (let [raw-max-value (sc/text (sc/select root [:#rand-mag]))]
    (when-let [max-value (g/parse-double raw-max-value)]

      (doseq [row (sc/select root [:.mult-panel :.mult-row])
              :let [locked? (-> row (sc/select [:.lock-check]) (first) (sc/selection))]]

          (when-not locked?
            (let [input (sc/select row [:.mult-input])]
              (sc/text! input
                (format "%.2f"
                        (g/random-double (- max-value) max-value global-rand-gen))))))

      (update-coloring! root color-atom canvas)

      (println (co/format-options (options-from-panel root))))))

(defn new-random-button [root class-selector color-atom canvas]
  (sc/button :text "Randomize", :font option-font,
             :listen [:action (partial random-button-handler root canvas color-atom)]))

(defn new-random-bar [root color-atom canvas]
  (let [rand-button (new-random-button root :.mult-input color-atom canvas)
        rand-max-entry (sc/text :text (str default-max-rand-mag), :font option-font
                                :columns 4, :halign :center
                                :id :rand-mag)]

    (sc/flow-panel :items [rand-button rand-max-entry])))

(defn new-lock-button [root text selection]
  (let [h (fn [_] (doseq [lock-check (sc/select root [:.lock-check])]
                    (sc/selection! lock-check selection)))]

    (sc/button :text text, :listen [:action h])))

(defn new-option-panel [canvas color-atom]
  (let [red-panel (new-color-mult-panel "Red")
        green-panel (new-color-mult-panel "Green")
        blue-panel (new-color-mult-panel "Blue")

        vert-panel (sc/vertical-panel)

        select-bar (sc/horizontal-panel :items [(new-lock-button vert-panel "Unlock" false)
                                                (new-lock-button vert-panel "Lock" true)])

        update-button (new-update-button canvas color-atom vert-panel)

        rand-bar (new-random-bar vert-panel color-atom canvas)]

    (sc/config! vert-panel
       :items [select-bar red-panel green-panel blue-panel update-button rand-bar])

    vert-panel))

(defn test-frame [width height canvas color-atom]
  (let [f (sc/frame :title "Color Picker", :size [width :by height]
                    :content (new-option-panel canvas color-atom))]
    f))