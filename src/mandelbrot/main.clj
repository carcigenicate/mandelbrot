(ns mandelbrot.main
  (:require [mandelbrot.seesaw.third-try :as ui]
            [seesaw.core :as sc]
            [helpers.general-helpers :as g])
  (:gen-class))

(defn -main [& [start-r? end-r? start-i? end-i? :as args]]
  (let [std-args (map str args)
        parsed (map g/parse-double std-args)
        valid? (every? identity parsed)

        good-args (if valid? parsed [])]

    (println std-args parsed valid? good-args)

    (-> (apply ui/frame good-args)
        (sc/show!))))