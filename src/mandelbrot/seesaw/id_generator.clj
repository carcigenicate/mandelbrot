(ns mandelbrot.seesaw.id-generator)

(defn new-id-generator [starting-id]
  (atom (dec starting-id)))

(defn generate-id! [generator]
  (swap! generator inc))