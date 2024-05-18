(defproject mandelbrot "2"
  :description "FIXME: write description"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [seesaw "1.5.0"]
                 [org.clojure/core.async "0.4.490"]]

  :main mandelbrot.main

  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}})
