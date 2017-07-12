(ns mandelbrot.future-test)

(defn new-result-ref []
  (ref []))

(defn flush-results [result-ref]
  (dosync
    (let [results @result-ref]
      (ref-set result-ref [])
      results)))

(defn add-result [result-ref result]
  (dosync
    (alter result-ref #(conj % result))))

(defn- start-task [task result-ref]
  (future
    (add-result result-ref
                (task))))

(defn start-tasks [tasks result-ref]
  (doseq [t tasks]
    (start-task t result-ref)))