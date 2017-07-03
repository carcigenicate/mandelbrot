(ns mandelbrot.concur-iter-finder
  (:require [mandelbrot.mandelbrot :as m])

  (:import  [java.util Collections Collections$SynchronizedCollection ArrayList]
            [java.util.concurrent ExecutorService Executors ConcurrentLinkedQueue]))

(def n-threads 6)

(defn new-executor-service []
  (Executors/newFixedThreadPool n-threads))

(def ex (atom (new-executor-service)))

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

      results)))

(defn create-finder-task [a b max-iters]
  (fn []
    (let [n (m/converges-at? a b max-iters)]
      (add-to-queue (->Point-data a b n)))))

(defn start-finding [points max-iters]
  (when-not @ex
    (reset! ex (new-executor-service)))

  (doseq [[a b] points]
    (.submit ^ExecutorService @ex
             ^Runnable (create-finder-task a b max-iters))))

(defn cancel-finding-all []
  (when @ex
    (.shutdownNow ^ExecutorService @ex))

  (reset! ex nil))

; The thread pool doesn't automatically shut down and prevents the JVM from closing
(.addShutdownHook
  (Runtime/getRuntime)
  (Thread. ^Runnable
           (fn []
             (cancel-finding-all))))