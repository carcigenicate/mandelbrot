(ns mandelbrot.main
  (:require [quil.core :as q]
            [quil.middleware :as mi]

            [mandelbrot.mandelbrot :as m]
            [mandelbrot.color-schemes :as c]

            [helpers.general-helpers :as g])



  (:gen-class))

(def width 2000)
(def height 1400)

(def mandel-min -1)
(def mandel-max 1)

(def max-tests 200)

(defn map-to-mandel [n]
  (q/map-range n 0 width mandel-min mandel-max))

(defn square-complex [a b]
    [(- (* a a)
        (* b b))

     (* 2 a b)])

(defn fz=z2+c [[zr zi] [cr ci]]
  (let [[r' i'] (square-complex zr zi)]
    [(+ r' cr) (+ i' ci)]))


; TODO: Terrible name?
(defn converges-at? [a b max-iters]
  (loop [n 0
         r a
         i b]
    (let [[r' i'] (fz=z2+c [r i] [a b])]
      (if (and (< n max-iters)
               (<= (Math/abs ^double (+ r' i')) 2))
        (recur (inc n) r' i')
        n))))

(defn int-to-color [n]
  (let [r (int (/ n (* 255 255 255)))
        n' (rem n 255)

        g (int (/ n' (* 255 255)))
        n'' (rem n' 255)

        b (int (/ n'' 255))]
    [r g b]))

(defn setup-state []
  (doseq [y (range height)
          x (range width)]

    (let [a (map-to-mandel x)
          b (map-to-mandel y)

          n (converges-at? a b max-tests)]

      (let [c (c/complex-purple n)]
        (q/with-stroke c
          (q/point x y)))))

  (println "DONE!"))

(defn update-state [state])

(defn draw-state [state])

(defn -main []
  (q/defsketch Mandel
               :size [width height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :middleware [mi/fun-mode]))


