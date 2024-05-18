(ns mandelbrot.helpers.general-helpers
  (:import [java.util Random Collections Collection]
           [java.math RoundingMode MathContext])
  (:refer-clojure :exclude [shuffle]))

(defn abs [^Double n]
  (Math/abs n))

(defn sqrt [^Double n]
  (Math/sqrt n))

(defn square [^Double n]
  (* n n))

(defn signum [^Double n]
  (Math/signum n))

(defmacro test-range [tries expr]
  `(let [gend# (sort
                 (for [_# (range ~tries)] ~expr))
         low# (first gend#)
         high# (last gend#)]
     [low# high#]))

(defn new-rand-gen
  ([seed] (Random. seed))
  ([] (Random.)))

(defn random-int
  "Returns a random integer (actually a long) between min (inclusive) and max (exclusive)."
  ^long [^long min ^long max ^Random rand-gen]
  (+ min (.nextInt rand-gen (- max min))))

(defn- random-floating [f ^double min ^double max ^Random rand-gen]
  (let [r (f rand-gen)
        spread (- max min)
        rand (* spread r)]
    (+ rand min)))

(defn random-float [min max ^Random rand-gen]
  (random-floating #(.nextFloat ^Random %) min max rand-gen))

(defn random-double ^double [^double min ^double max ^Random rand-gen]
  (random-floating #(.nextDouble ^Random %) min max rand-gen))

(defn random-boolean [^Random rand-gen]
  (.nextBoolean rand-gen))

(defn random-perc [^double perc-chance ^Random rand-gen]
  (<= (.nextDouble rand-gen) perc-chance))

(defn random-from-collection
  "Returns a random element from the collection.
  Converts the collection into an Persistant List if the collection isn't a sequence or vector, so it may be slow for large collections
  Returns nil if given an empty list"
  [coll ^Random rand-gen]
  (if (empty? coll)
    nil
    (let [rand-i (random-int 0 (count coll) rand-gen)
          l (if (or (seq? coll) (vector? coll))
              coll
              (into '() coll))]
      (nth l rand-i))))

(defn shuffle [^Collection coll ^Random rand-gen]
  (let [j-coll (java.util.ArrayList. coll)]
    (Collections/shuffle j-coll rand-gen)
    (into (empty coll) j-coll)))


(defn deviate-by-perc
  "Deviates a number by the given decimal percentage.
  (deviate 100 0.5 rand-gen) would give back a number in the range [75 125]
  The result will match the number type of the input, but will only deviate by integer values"

  ([n perc ^Random rand-gen]
   (let [max-dev (/ (* n perc) 2)
         dev (random-double 0 max-dev rand-gen)
         sign (if (random-boolean rand-gen) 1 -1)]
    (+ n (* sign dev)))))

(defn deviate [n max-deviation ^Random rand-gen]
  (random-double
    (- n max-deviation)
    (+ n max-deviation)
    rand-gen))

(defn current-nano-timestamp []
  (. System (nanoTime)))

(defn current-ms-timestamp []
  (double (/ (current-nano-timestamp) 1000000)))

(defmacro time-pure
  "Evaluates expr and returns the time it took.
  Modified the native time macro to return the time taken."
  [expr]
  `(let [start# (current-nano-timestamp)
         ret# ~expr]
     (/ (double (- (current-nano-timestamp) start#)) 1000000.0)))


(defmacro time-multiple
  "Times the expression multiple times, returning the total time taken, in ms"
  [times expr]
  `(time-pure
     (dotimes [_# ~times]
      ~expr)))

(defmacro time-multiple-avg
  "Times the expression multiple times, returning the average time taken, in ms"
  [times expr]
  `(double (/
            (time-multiple ~times ~expr)
            ~times)))

(defmacro dbg
  "Prints the wrapped expression, and returns the result.
  Idea taken from 'Learning Clojure'"
  [expr]
  `(let [e# ~expr]
     (clojure.pprint/pprint (str '~expr " = " e#))
     e#))

; Cool, but expands huge.
(defmacro time-mutliple-compare-avg [times & exprs]
  `(map
     (fn [[e# t#]] [e# (/ t# ~times)])
     ~(reduce
       (fn [acc expr]
         (update acc (str expr)
           #(let [time-m `(time-pure ~expr)]
              (if (nil? %) time-m `(+ ~time-m ~%)))))
       {}
       (take (* times (count exprs)) (cycle exprs)))))

(defmacro time-each [& exprs]
  `(vector ~@(for [e exprs]
               `(time-pure ~e))))


(defmacro force-dbg
  "Forces the expr to be evaluated using doall
  Must evaluate to a lazy seq, or an exception will be thrown.
  UNTESTED"
  [expr]
  `(let [e# ~expr]
     (println (str (doall '~expr) " = " e#))
     e#))

(defmacro pretty-expand [macro]
  `(clojure.pprint/pprint
     (macroexpand '~macro)))

(defmacro pretty-expand-all [macro]
  `(clojure.pprint/pprint
     (clojure.walk/macroexpand-all '~macro)))

(defn test-perc [num-of-tests pred f]
  (double
      (/
        (reduce (fn [acc _]
                  (if (pred (f))
                    (inc acc)
                    acc))
                0 (range num-of-tests))
        num-of-tests)))

; Taken from http://clojure-doc.org/articles/language/functions.html
(defn prec-round
  "Round a double to the given precision (number of significant digits)"
  [precision ^double d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn clamp "Returns the number clamped to the given range; inclusive"
  [n min max]
  (cond
    (< n min) min
    (> n max) max
    :else n))

(defn wrap
  "Wraps n so it's between min-n (inclusive) and max-n (exclusive).
  WARNING: Replaces a version that used an exclusive upper bound.
           The max-n may need to be adjusted to account for that."
  [n min-n max-n]
  (let [limit (- max-n min-n)]
    (+ (mod n limit) min-n)))

(defn map-range [value start1 stop1 start2 stop2]
  (+ start2
     (* (- stop2 start2)
        (/ (- value start1)
           (- stop1 start1)))))

(defn point-within-circle? [x y circle-center-x circle-center-y circle-radius]
  (let [x-diff (abs (- x circle-center-x))
        y-diff (abs (- y circle-center-y))]
    (<= (+ (* x-diff x-diff)
           (* y-diff y-diff))
        (* circle-radius circle-radius))))

(defn parse-int
  "Returns nil on bad input"
  [str-n]
  (try
    (Integer/parseInt str-n 10)
    (catch NumberFormatException _
      nil)))

(defn parse-double
  "Returns nil on bad input"
  [str-n]
  (try
    (Double/parseDouble str-n)
    (catch NumberFormatException _
      nil)))

(defn distance-between-points
  ([x1 y1 x2 y2]
   (let [x-off (- x1 x2)
         y-off (- y1 y2)]
     (Math/sqrt (+ (* x-off x-off) (* y-off y-off)))))

  ([[x1 y1] [x2 y2]]
   (distance-between-points x1 y1 x2 y2)))
   
(defn number-suffix [n]
  (let [ones (last (str n))]
    (case ones
      \1 "st"
      \2 "nd"
      \3 "rd"
      "th")))

(defn enumerate
  "Returns a lazyseq of pairs, each representing [item-index item]."
  [coll]
  (map vector (range) coll))

(defn iterate-many [x repetitions iterate-f]
  (reduce
    (fn [acc _] (iterate-f acc))
    x
    (range repetitions)))

; RIP
#_
(defn lsubseq
  "Lazily sub-sequences any iterable collection.
  The left-index is inclusive, while the right is exclusive."
  [coll left-index right-index]
  (map second
    (filter #(<= left-index (first %) (dec right-index))
      (enumerate coll))))

; http://codereview.stackexchange.com/questions/144068/lazy-sub-sequence
(defn lsubseq
  "Lazily sub-sequences any iterable collection.
  The left-index is inclusive, while the right is exclusive."
  [coll left-index right-index]
  (take (- right-index left-index) (drop left-index coll)))
  
(defn remove-first
  "Removes the first instance of x from xs."
  [xs x]
  (let [[n m] (split-with #(not= x %) xs)]
    (concat n (rest m))))

(defn ask-for-input
  "Prompts the user for input, checks it using the validation function, and displays the error if the validation fails. Newlines aren't added after the messages.
  If an empty string is "
  [prompt-message error-message validate-f]
  (print prompt-message)
  (flush)

  (let [result (read-line)]
    (if (validate-f result)
      result
      (do
        (print error-message)
        (flush)

        (recur prompt-message error-message validate-f)))))

(defn numeric?
  "Checks if the string represents a valid integer number.
  A number is considered valid only if all of it's digits are numbers (not including a dash as the first character,
  which is allowed)."
  [str-n]
  (every? #(Character/isDigit ^Character %)
          (if (= (first str-n) \-)
            (rest str-n)
            str-n)))

(defn inc-permutation
  "\"Counts\" arbitrary symbols.
  Example: (inc-permutation \\a \\c #(char (inc (int %) [\\a \\b \\b])
  returns [\\a \\b \\c], then [\\a \\c \\a], [\\a \\c \\b], [\\a \\c \\c], [\\b \\a \\a], [\\b \\a \\lein pomb]...
  Quite slow."
  [first-symbol last-symbol inc-f current-permutation]
  (let [current-ones (last current-permutation)
        carry? (= current-ones last-symbol)
        overflown? (empty? current-permutation)
        last-i (dec (count current-permutation))]

    (cond
      carry? (conj
               (inc-permutation first-symbol last-symbol inc-f (subvec current-permutation 0 last-i))
               first-symbol)

      overflown? []

      :else (assoc current-permutation last-i (inc-f current-ones)))))

(defn thread*
  "Runs f in a new thread."
  [^Runnable f] ^Thread
  (doto (Thread. f)
    (.start)))

(defmacro thread
  "Runs the body in a new thread."
  [& body]
  `(thread*
     (fn [] ~@body)))

(defn timeout*
  "Wait for timeout-t many milliseconds for f to finish.
  Returns default if it times out."
  [timeout-t default f]
  (let [fut (future (f))
        cur-time current-ms-timestamp
        start-t (cur-time)
        sleep-t (/ timeout-t 10)]
    (loop []
      (Thread/sleep sleep-t)
      (if (>= (- (cur-time) start-t) timeout-t)
        default
        (if (realized? fut)
          @fut
          (recur))))))

(defmacro timeout
  "Wait for timeout-t many milliseconds for the body to finish.
  Returns default if it times out."
  [t default & body]
  `(timeout* ~t ~default (fn [] ~@body)))