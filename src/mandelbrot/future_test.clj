(ns mandelbrot.future-test)

(defrecord Results-Manager [result-ref futures])

(defn- flush-results [result-ref]
  (dosync
    (let [results @result-ref]
      (ref-set result-ref [])
      results)))

(defn- add-result-to-ref [result-ref result]
  (dosync
    (alter result-ref #(conj % result))))

(defn pop-results [r-mgr]
  (flush-results
    (:result-ref r-mgr)))

(defn add-result [r-mgr result]
  (add-result-to-ref (:result-ref r-mgr)
                     result))

(defn- cancel-and-remove-future [r-mgr future]
  (future-cancel future)
  (update r-mgr :futures #(disj % future)))

(defn- cancel-and-remove-futures [r-mgr futures]
  (reduce cancel-and-remove-future r-mgr futures))

(defn cancel-finding [r-mgr]
  (cancel-and-remove-futures r-mgr (:futures r-mgr)))

(defn- start-task [task r-mgr]
  (let [result-ref (:result-ref r-mgr)
        fut (future
              (add-result result-ref
                          (task)))]))

(defn start-tasks [r-mgr tasks]
  (let [result-ref (:result-ref r-mgr)
        futs (map #(start-task % result-ref) tasks)]
    (->Results-Manager result-ref futs)))