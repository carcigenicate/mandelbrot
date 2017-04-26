(ns mandelbrot.concur-iter-finder
  (:require [mandelbrot.mandelbrot :as m])

  (:import  [java.util Collections Collections$SynchronizedCollection ArrayList]
            [java.util.concurrent ExecutorService Executors ConcurrentLinkedQueue]))

(def ex (Executors/newFixedThreadPool 6))

(def draw-queue (Collections/synchronizedCollection (ArrayList.)))

(defrecord Point-data [a b n]
  Object
  (toString [self] (str "["a "," b "]:" n)))

(defn add-to-queue [point-data]
  (.add ^Collections$SynchronizedCollection draw-queue
        point-data))

(defn grab-and-clear-queue []
  (locking draw-queue
    (let [results (into [] draw-queue)]
      (.clear ^Collections$SynchronizedCollection draw-queue)

      (println (count results))

      results)))

(defn create-finder-task [a b max-iters]
  (fn []
    (let [n (m/converges-at? a b max-iters)]
      (add-to-queue (->Point-data a b n)))))

(defn start-finding [points max-iters]
  (doseq [[a b] points]
    (.submit ^ExecutorService ex
             ^Runnable (create-finder-task a b max-iters))))

(.addShutdownHook
  (Runtime/getRuntime)
  (Thread. ^Runnable (fn [] (.shutdownNow ex))))