(ns mandelbrot.future-results
  (:import [java.util Collections Collections$SynchronizedCollection ArrayList])

  (:gen-class))

(defn new-result-container []
  (Collections/synchronizedCollection (ArrayList.)))

(defn flush-results [result-coll]
  (locking result-coll
    (let [results (into [] result-coll)]
      (.clear ^Collections$SynchronizedCollection result-coll)
      results)))

(defn add-result [result-coll result]
  (.add ^Collections$SynchronizedCollection result-coll result))

(defn- start-task [task result-ref]
  (future
    (add-result result-ref
                (task))))

(defn start-tasks
  "Runs all tasks in a new future."
  [tasks result-coll]
  (future
    (mapv
      #(add-result result-coll (%))
      tasks)))

(defn -main []
  (println "Press enter")
  (read-line)

  (let [rc (new-result-container)
        r (vec (range 800))
        total-tasks 5331200 ; 5331200
        chunk-size (int (inc (/ total-tasks 1000)))
        t (fn [i] (apply conj [] r) i)
        ts (map #(fn [] (t %)) (range total-tasks))
        chunks (vec (partition chunk-size ts))]

    (println "Starting to find" (count chunks) "," chunk-size "sized chunks.")

    (time
      (doseq [[i chunk] (map vector (range) chunks)]
        (println "Starting chunk #" i "\n")
        (start-tasks chunk rc)
        (println "\tTask Started...")
        (Thread/sleep 100)))))

