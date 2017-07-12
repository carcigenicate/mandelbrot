(ns mandelbrot.loremipsum
  (:require [clojure.string :as s]))

; TODO: Move to bits project

(defn rand-word
  "Generates a random word based on what letters appeared at what index.
  If the index doesn't exist, it pulls a random character from the sample-text."
  [length pos-map sample-text]
  (apply str
    (for [i (range length)]
      (rand-nth (get pos-map i sample-text)))))

(defn safe-rand-word
  "Helper that loops while the produced input is in the exclusion set.
  May get stuck if the sample size is too small."
  [length pos-map sample-text exclusion-set]
  (loop []
    (let [word (rand-word length pos-map sample-text)]
      (if (exclusion-set word)
        (recur)
        word))))

(defn rand-word-len
  "Generates a length +/- by deviating the average length bysome amount within
  the (range (- deviation) deviation))."
  [avg-len deviation]
  (+ avg-len
     (rand-nth (range (- deviation) deviation))))

(defn pos-letters
  "Records what characters occured at each index of each word."
  [words]
  ; A two dimensional reduction.
  ; The inner reduction takes an existing pos-map, and updates it with information about the word
  ; The outer reduction combines all the word pos-maps together.
  (reduce (fn [pos-map word]
            (reduce (fn [acc [i chr]]
                      (update acc i #(if % (conj % chr)
                                           [chr])))
                    pos-map
                    (map vector (range) word)))
          {}
          words))

(defn gobbly [n-chars sample-text]
  (let [; Split the text into words, the place the words into a set
        ;  to ensure that they aren't generated.
        words (s/split sample-text #"\s")
        word-set (into #{} words)

        stripped-text (s/replace sample-text #"[,.?!]" "")

        ; Figure out the average word length, then calculate how much
        ;  deviation should be allowed.
        avg-word-len (/ (count sample-text) (count words))
        length-deviation (/ avg-word-len 4)

        ; Find which characters appear at which positions
        pos-map (pos-letters words)]

    ; Generate n-chars many characters by...
    (doseq [n (range n-chars)]
      ; ...printing a space, then a randomly generated word.
      (print ""
        (safe-rand-word (rand-word-len avg-word-len length-deviation)
                        pos-map
                        stripped-text
                        word-set)))

    (flush)))