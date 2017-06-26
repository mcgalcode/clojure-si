(ns citrine-challenge.parser
  (:require [citrine-challenge.si :as si]
            [clojure.string :as str]))

(def valid-operators
  ["(" ")" "*" "/"])

(defn is-valid-operator?
  [operator]
  (some #{(str operator)} valid-operators))

(defn is-valid-unit?
  [unit-name]
  (get si/units (str unit-name)))

(defn split-unit-str-to-chars
  [unit-expr-str]
  (filter #(> (count %) 0) (map str (str/split unit-expr-str #""))))

(defn get-unit-multiplier
  [unit-name]
  (get (get si/units (str unit-name)) :multiplier))

(defn get-unit-si-name
  [unit-name]
  (get (get si/units (str unit-name)) :name))

(defn is_paren?
  [op]
  (some #{(str op)} [")" "("]))

(defn get-first-expr-block
  "Iterates through a vector and returns the first character if its an operator, or everything up until the first operator"
  [expr-block-array]
  (if (empty? expr-block-array)
    [nil nil]
    (if (is-valid-operator? (first expr-block-array))
      [(first expr-block-array) (rest expr-block-array)]
      (loop [remaining-expr-block expr-block-array
             unit-name-vec []]
        ;; If the next character is a valid operator or 
        (if (or (is-valid-operator? (first remaining-expr-block))
             (nil? (first remaining-expr-block)))
          [(str/join "" unit-name-vec) remaining-expr-block]
          (recur (rest remaining-expr-block)
            (conj unit-name-vec (str (first remaining-expr-block)))))))))

(defn get-unit-op-vec
  [unit-expr-str]
  (let [in-char-vec (split-unit-str-to-chars unit-expr-str)]
    (loop [remaining-char-vec in-char-vec
           uovec []]
      (let [[first-expr remaining-chars] (get-first-expr-block remaining-char-vec)]
        (if (nil? first-expr)
          uovec
          (recur remaining-chars
            (conj uovec first-expr)))))))

(defn is-valid-op-or-unit?
  [arg]
  (or (is-valid-unit? arg)
      (is-valid-operator? arg)))

(defn is-valid-input?
  [input-str]
  (every? is-valid-op-or-unit? (get-unit-op-vec input-str)))

(defn op-string-to-func
  [opstr]
  (cond
    (= opstr "*") *
    (= opstr "/") /))

; ["(" "min" "*" "deg" ")" "/" "(" "ha" "*" "L" ")"] -> [ ["min" "*" "deg"] ["/" "(" "ha" "*" "L" ")"] ]
(defn separate-first-paren-block
  "Takes a string that starts with an open parenthese and returns the content inside that parenthese block"
  [uovec]
  (loop [paren-stack 1
         ; should be a single open paren
         paren-block []
         remaining-vec (rest uovec)]
    (if (= paren-stack 0)
      [paren-block remaining-vec]
      (cond
        (= (first remaining-vec) "(") (
          recur (inc paren-stack)
            (conj paren-block (first remaining-vec))
            (rest remaining-vec))
        (and (= (first remaining-vec) ")")
          (= (dec paren-stack) 0 )) [paren-block (rest remaining-vec)]
        (= (first remaining-vec) ")") (
          recur (dec paren-stack)
            (conj paren-block (first remaining-vec))
            (rest remaining-vec))
        :else (recur paren-stack
            (conj paren-block (first remaining-vec))
            (rest remaining-vec)))
      )))

(defn collapse-uovec-to-multiplier
  "Converts a valid unit string to a multiplier"
  [uovec]  
  (if (= (count uovec) 1)
    (get-unit-multiplier (first uovec))
    (let [[first-item & operator-first-uovec] uovec]
      ; at this point operator-first-uovec must be at least length 3
      (loop [[operator & remaining-items] operator-first-uovec
             multiplier (get-unit-multiplier first-item)]
        (if (= (first remaining-items) "(")
          (let [[paren-block after-block-tail] (separate-first-paren-block remaining-items)]
            (let [new-multiplier ((op-string-to-func operator) multiplier (collapse-uovec-to-multiplier paren-block))]
              (if (not (> (count after-block-tail) 0))
                new-multiplier
                (recur after-block-tail
                  new-multiplier)
                )))
          (let [new-multiplier 
              ((op-string-to-func operator) multiplier (get-unit-multiplier (first remaining-items)))]
            (if (> (count remaining-items) 1)
              (recur (rest remaining-items)
                new-multiplier)
              new-multiplier)))))))

(defn convert-unit-or-op-to-si
  [uostr]
  (cond
    (is-valid-operator? uostr) uostr
    (is-valid-unit? uostr) (get-unit-si-name uostr)
    :else ""))

(defn make-si-unit-name-vec
  [uovec]
  (loop [remaining-items uovec
         si-vec []]
    (if (= (count remaining-items) 0)
      si-vec
      (recur (rest remaining-items)
        (conj si-vec (convert-unit-or-op-to-si (first remaining-items))))
      )))

(defn make-si-unit-name-string
  [input-str]
  (str/join "" (make-si-unit-name-vec (get-unit-op-vec input-str))))

(defn calculate-si-unit-multiplier
  [input-str]
  (collapse-uovec-to-multiplier (get-unit-op-vec input-str)))

(defn get-conversion
  [input-str]
  { 
    :si-equivalent {
      :name-string (make-si-unit-name-string input-str)
      :multiplier (calculate-si-unit-multiplier input-str)
    }
  })