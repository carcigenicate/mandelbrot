(ns mandelbrot.future-results
  (:import [java.util Collections Collections$SynchronizedCollection ArrayList])

  (:gen-class))

(defrecord Collector [continue? result-coll])

(defn new-result-collector []
  (->Collector (atom true)
               (Collections/synchronizedCollection (ArrayList.))))

(defn- flush-results [result-coll]
  (locking result-coll
    (let [results (into [] result-coll)]
      (.clear ^Collections$SynchronizedCollection result-coll)
      results)))

(defn get-results [collector]
  (flush-results (:result-coll collector)))

(defn add-result [result-coll result]
  (.add ^Collections$SynchronizedCollection result-coll result))

(defn- initiate-tasks [collector]
  (reset! (:continue? collector) true))

(defn cancel-tasks [collector]
  (reset! (:continue? collector) false))

(defn start-task [collector task]
  (initiate-tasks collector)

  (future
    (add-result (:result-coll collector)
                (task))))

(defn start-tasks
  "Runs all tasks in a new future."
  [collector tasks]
  (initiate-tasks collector)

  (let [{rc :result-coll c? :continue?} collector]
    (future
      (reduce (fn [_ task]
                (if @c?
                  (add-result rc (task))
                  (reduced nil)))

              tasks))))

(defn -main []
  (println "Press enter")
  (read-line)

  (let [rc (new-result-collector)
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

