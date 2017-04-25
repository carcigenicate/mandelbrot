(ns mandelbrot.ex-test
  (:import [java.util.concurrent ExecutorService Executors]))

(def queue (atom []))

(def ex (Executors/newFixedThreadPool 4))

(def ex-delay 250)
(def n-tasks 1000)

(defn add-to-queue [result]
  (swap! queue #(conj % result)))

(defn task [result]
  (Thread/sleep 250)

  (add-to-queue result))

(defn create-task [result]
  (fn [] (task result)))

(defn create-expensive-tasks [n]
  (vec (for [x (range n)]
         (create-task (rand-int 1000)))))

(defn start-tasks [n]
  (let [tasks (create-expensive-tasks n)]
    (doseq [task tasks]
      (.submit ex ^Runnable task))))