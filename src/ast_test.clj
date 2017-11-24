(ns ast-test
  (:require [clojure.string :as s]))

(defrecord Binary-Op [op left-op right-op])
(defrecord Literal [value])

(def bin-ops #{+ * - /})

(def test-eq "1 + 2 + 3")
(def test-raw-res (->Binary-Op "+"
                    (->Binary-Op "+"
                      (->Literal "1")
                      (->Literal "2"))
                    (->Literal "3")))

(defn parse-equation [^String equation]
  (let [[origin-literal & rest-equation-symbols] (s/split equation #" ")]

    (loop [acc-syms rest-equation-symbols
           acc-tree (->Literal origin-literal)]

      (if (empty? acc-syms)
        acc-tree

        (let [[[op lit] rest-syms] (split-at 2 acc-syms)]
          (recur rest-syms
            (->Binary-Op op
                         acc-tree
                         (->Literal lit))))))))

(defn evaluate-ast [])