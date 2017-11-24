(ns parser.shunting
  (:require [clojure.string :as s]
            [helpers.general-helpers :as g])

  (:import [java.text ParseException]))

; FIXME: Because of the definition of numeric?, this only handles integers currently

(defrecord Operator-Attribute [precedence left-assoc?])

(def new-op-attr ->Operator-Attribute)

(defn- parsing-error ^IllegalArgumentException [^String message]
  (IllegalArgumentException.
    (str "Problem while parsing: " message)))

(defn- parse-double?
  "Tries to parse the String as a double.
  Returns nil on failure.
  Interprets sequences such as \"1e5\" as scientific notation."
  [^String n]
  (try
    (Double/parseDouble n)
    (catch NumberFormatException e nil)))

(defn numeric?
  "If given a char, it's checked using Character/isDigit.
  If given a string, it returns the result of giving the string to parse-double?"
  [str-or-char]
  (if (char? str-or-char)
    (Character/isDigit ^Character str-or-char)
    (parse-double? str-or-char)))

(def ^:private left-bracket?
  #{"(" "[" "{"})

(def ^:private right-bracket?
  #{")" "]" "}"})

(defn- resolve-right-brace
  "Pops the stack onto the output queue while the popped operator isn't a left-bracket.
  Returns a pair of [popped-stack updated-output].
  Throws an IllegalArgumentException if a left bracket isn't found."
  [op-stack output-queue]
  (loop [[popped-op & rest-stack] op-stack
         acc-output output-queue]
    (cond
      (nil? popped-op)
      (throw (parsing-error "Mismatched right bracket."))

      (left-bracket? popped-op)
      [rest-stack acc-output]

      :else
      (recur rest-stack (conj acc-output popped-op)))))

(defn- resolve-operator
  "Pops operators from the stack to the output queue while the popped operators are left associative and have a higher precedence than op.
  Returns a pair of [popped-stack updated-output]"
  [op op-stack output-queue op-attr-map]
  (let [{op-prec :precedence} (op-attr-map op)]
    (loop [[popped-op & rest-ops :as acc-stack] op-stack
           acc-output output-queue]
      (if popped-op
        (let [{pop-op-prec :precedence, pop-op-l-assoc? :left-assoc?}
              (op-attr-map popped-op)
              pushed-output (conj acc-output popped-op)]

          (if (and pop-op-l-assoc? (>= pop-op-prec op-prec))
            (recur rest-ops pushed-output)
            [(conj acc-stack op) acc-output]))

        [(conj rest-ops op) acc-output]))))

(defn- infix->RPN-tokens [tokens op-attr-map]
  (loop [op-stack '()
         [tok & rest-toks] tokens
         output []]
    (cond
      (nil? tok)
      (if (some left-bracket? op-stack)
        (throw (parsing-error "Mismatched left bracket."))
        (into output op-stack))

      (numeric? tok)
      (recur op-stack rest-toks (conj output tok))

      (left-bracket? tok)
      (recur (conj op-stack tok) rest-toks output)

      (right-bracket? tok)
      (let [[updated-stack updated-output]
            (resolve-right-brace op-stack output)]

        (recur updated-stack rest-toks updated-output))

      :else ; Must be an operator. Check against op-attr-map?
      (let [[updated-stack updated-output]
            (resolve-operator tok op-stack output op-attr-map)]

        (recur updated-stack rest-toks updated-output)))))

(defn- remove-first-token
  "Takes a string equation, and splits it on the first token.
  Returns a pair of [token remaining-equation]"
  [^String remaining-equation]
  (let [trimmed (s/triml remaining-equation)
        base-tok (first trimmed)]
    (if (numeric? base-tok)
      (let [pieces (split-with numeric? trimmed)]
        (mapv (partial apply str) pieces))

      [(str base-tok) (subs trimmed 1)])))

(defn- tokenize-equation
  "Takes a string equation and returns a vector of tokens."
  [^String equation]
  (loop [acc-equ equation
         tokens []]
    (if (empty? acc-equ)
      tokens
      (let [[tok rest-equ] (remove-first-token acc-equ)]
        (recur rest-equ (conj tokens tok))))))

(defn- tokens-to-string [tokens]
  (s/join " " tokens))

(defn infix->RPN
  "Takes a equation, and an op-map, and converts the infix equation into RPN representation based on the operator precedences defined by the op-map.
  The op-map is a map of string->Operator-Attribute pairs, where the string is the representation of the operator."
  [^String equation, op-attr-map]
  (-> equation
      (tokenize-equation)
      (infix->RPN-tokens op-attr-map)
      (tokens-to-string)))

(defn evaluate-operator [^String op, str-args, op-def-map]
  (if-let [op-f (op-def-map op)]
    ; str-args must be parsable for them to have been added to the stack
    (apply op-f (map parse-double? str-args))

    (throw (parsing-error (str "Operator " op " undefined.")))))

(defn process-operator [^String op, result-stack, op-def-map]
  (let [[arg2 arg1 & rest-stack] result-stack] ; Args are intentionally backwards
    (if (and arg1 arg2)
      (let [result (evaluate-operator op [arg2 arg1] op-def-map)]
        (conj rest-stack (str result)))

      (throw (parsing-error (str "Not enough arguments to evaluate " op))))))

(defn evaluate-RPN-tokens [rpn-tokens op-def-map]
  (loop [[tok & rest-toks] rpn-tokens
         result-stack '()]

    (cond
      (nil? tok)
      (if (= (count result-stack) 1)
        (first result-stack)
        (throw (parsing-error (str "Too many arguments given. Remaining arguments: "
                                   (drop 1 result-stack)))))

      (numeric? tok)
      (recur rest-toks (conj result-stack tok))

      :else
      (let [updated-stack (process-operator tok result-stack op-def-map)]
        (recur rest-toks updated-stack)))))

(def ^:private test-op-attr-map
  {"+" (new-op-attr 1 true),
   "-" (new-op-attr 1 true),
   "*" (new-op-attr 2 true),
   "/" (new-op-attr 2 true)})

(defn stress-test []
  (let [ops (mapv str "+-*/")

        equation-toks
        (for [i (range 1 100)]
          (if (even? i)
            (rand-nth ops)
            (str (inc (rand-int 20)))))

        rpn-toks
        (infix->RPN-tokens equation-toks test-op-attr-map)

        result
        (evaluate-RPN-tokens rpn-toks {})]

    result))
