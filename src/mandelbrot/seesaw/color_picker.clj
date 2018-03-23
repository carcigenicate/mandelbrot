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
#_
(defn effect-all-options [root class-selector]
  (let [handler (fn [f _]
                  (affect-all-props root class-selector :text
                    #(if-let [n? (g/parse-double %)]
                       (f n?)
                       %)))

        b #(sc/button :text (str %),
                      :listen [:action (partial handler %2)])]

    (sc/flow-panel :items [(b "+" #(* % effect-all-mult))
                           (b "-" #(/ % effect-all-mult))])))

(defn new-color-mult-panel [label]
  (let [l #(sc/label :text (str %), :font option-font
                     :border 0)

        t #(sc/text :text (str starting-mult), :font option-font,
                    :columns 4, :halign :center, :class :mult-input)

        f #(sc/flow-panel :items [(l %) (t)], :align :center)

        main-options (sc/vertical-panel :items [(l label) (f "r") (f "i") (f "n")
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

(defn new-random-button [root class-selector color-atom canvas]
  (let [h (fn [_]
            (let [raw-max-value (sc/text (sc/select root [:#rand-mag]))]
              (when-let [max-value (g/parse-double raw-max-value)]
                (affect-all-props root class-selector :text
                  (fn [_] (format "%.2f"
                                  (g/random-double (- max-value) max-value global-rand-gen)))))))]))

                (update-coloring! root color-atom canvas)

                (println (co/format-options (options-from-panel root)))

    (sc/button :text "Randomize", :font option-font,
               :listen [:action h])

(defn new-random-bar [root color-atom canvas]
  (let [rand-button (new-random-button root :.mult-input color-atom canvas)
        rand-max-entry (sc/text :text (str default-max-rand-mag), :font option-font
                                :columns 4, :halign :center
                                :id :rand-mag)]

    (sc/flow-panel :items [rand-button rand-max-entry])))

(defn new-option-panel [canvas color-atom]
  (let [red-panel (new-color-mult-panel "Red")
        green-panel (new-color-mult-panel "Green")
        blue-panel (new-color-mult-panel "Blue")

        vert-panel (sc/vertical-panel :items [red-panel green-panel blue-panel])

        update-button (new-update-button canvas color-atom vert-panel)

        rand-bar (new-random-bar vert-panel color-atom canvas)]

    (sc/add! vert-panel update-button rand-bar)

    vert-panel))

(defn test-frame [width height canvas color-atom]
  (let [f (sc/frame :title "Color Picker", :size [width :by height]
                    :content (new-option-panel canvas color-atom))]
    f))