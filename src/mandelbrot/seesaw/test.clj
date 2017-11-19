(ns mandelbrot.seesaw.test
  (:require [seesaw.core :as sc])
  (:import (java.util Date)))


(defn new-menu-bar []
  (sc/menubar :items
              [(sc/label :text "Close on save?"),
               (sc/checkbox :id :close-on-save?-checkbox)]))


(defn frame []
  (let [bp (sc/border-panel :center (sc/label :text "Text!"))

        f (sc/frame :size [500 :by 500], :content bp
                    :menubar (new-menu-bar))]


    (sc/show! f)

    nil))