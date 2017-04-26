(ns mandelbrot.concur-iter-finder
  (:require [mandelbrot.mandelbrot :as m])
  (:import [java.util.concurrent ExecutorService Executors]))

(def ex (Executors/newFixedThreadPool 6))

(defrecord Point-data [a b n]
  Object
  (toString [self] (str "["a "," b "]:" n)))

(def draw-queue (ref []))

(defn add-to-queue [n]
  (dosync
    (alter draw-queue #(conj % n))))

(defn grab-and-clear-queue []
  (dosync
    (let [results @draw-queue]
      (ref-set draw-queue [])
      (println (count results))
      results)))

(defn create-finder-task [a b max-iters]
  (fn []
    (let [n (m/converges-at? a b max-iters)]
      (add-to-queue (->Point-data a b n)))))

(defn start-finder [points max-iters]
  (doseq [[a b] points]
    (.submit ^ExecutorService ex
             ^Runnable (create-finder-task a b max-iters))))