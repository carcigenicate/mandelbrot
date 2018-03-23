(ns mandelbrot.color-options
  (:require [helpers.general-helpers :as g])
  (:import [java.awt Color]))

(defrecord Color-Mults [r i n])
(defrecord Color-Options [red-mults green-mults blue-mults])

(defn new-color-options [red-r red-i red-n, green-r green-i green-n, blue-r blue-i blue-n]
  (->Color-Options
    (->Color-Mults red-r red-i red-n)
    (->Color-Mults green-r green-i green-n)
    (->Color-Mults blue-r blue-i blue-n)))

(defn flatten-color-options [color-options]
  (flatten (mapv vals (vals color-options))))

(defn format-options [options]
  (str (vec (flatten-color-options options))))

(defn new-basic-color-f [options]
  (let [{:keys [red-mults green-mults blue-mults]} options
        str-opts (format-options options)
        w #(g/wrap % 0 255)

        f (fn [r i n]
            (let [v (fn [mults]
                      (let [{rm :r, im :i, nm :n} mults]
                        (w (+ (* r rm)
                              (* i im)
                              (* n nm)))))]

              (Color. ^long (v red-mults)
                      ^long (v green-mults)
                      ^long (v blue-mults))))]

    ; So we can figure out what coloring was used later
    (with-meta f {:color-opts str-opts})))