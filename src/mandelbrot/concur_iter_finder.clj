(ns mandelbrot.concur-iter-finder
  (:require [mandelbrot.mandelbrot :as m])
  (:import [java.util.concurrent ExecutorService Executors]))

(def ex (Executors/newFixedThreadPool 4))

(def draw-queue-A (atom []))

(defn add-to-queue [n]
  (swap! draw-queue-A #(conj % n)))

(defn grab-and-clear-queue []
  (let [results (atom [])]
    (swap! draw-queue-A
           (fn [res] (reset! results res)
                     []))
    results))

(defn create-finder-task [a b max-iters] ^Runnable
  (fn []
    (let [n (m/converges-at? a b max-iters)]
      (add-to-queue n))))

(defn start-finder [points scre])