(ns citrine-challenge.tests
  (:require [citrine-challenge.parser :as parser]
            )
  (:use [clojure.test]))

(def short-first-simple-input
  "L*minute")

(def one-paren-input
  "L*(minute/ha)")

(def long-first-simple-input
  "minute*L")

(def mult-first-partial-input
  "*minute")

(def div-first-partial-input
  "/minute")

(def one-unit-string
  "minute")

(def short-first-complex-input
  "ha*degree/(minute*tonne/')")

(def invalid-plus-input
  "ha*degree/(min+tonne)")

(def invalid-bad-unit-input
  "'/tonne*(day*year)")

(deftest valid-operator-contains-division
  (is (some #{"/"} parser/valid-operators)))

(deftest identifies-valid-operators
  (is (parser/is-valid-operator? "/"))
  (is (parser/is-valid-operator? "*"))
  (is (parser/is-valid-operator? ")"))
  (is (parser/is-valid-operator? "("))
  (is (not (parser/is-valid-operator? "")))
  (is (not (parser/is-valid-operator? "+"))))

(deftest identifies-valid-unit
  (is (parser/is-valid-unit? "second"))
  (is (parser/is-valid-unit? "t"))
  (is (not (parser/is-valid-unit? "")))
  (is (not (parser/is-valid-unit? "mile"))))

(deftest splits-input-str
  (let [split-input (parser/split-unit-str-to-chars short-first-simple-input)]
    (is (= (count split-input) 8))
    (is (= (first split-input) "L")))
  (let [split-input (parser/split-unit-str-to-chars "")]
    (is (= split-input []))))  

(deftest it-retrieves-the-first-expression
  (is (nil? (first (parser/get-first-expr-block []))))
  (let [[first-found-val rest-of-string] (parser/get-first-expr-block (parser/split-unit-str-to-chars short-first-simple-input))]
    (is (= first-found-val "L"))
    (is (= (first rest-of-string) "*"))
    (is (= (count rest-of-string) 7)))
  (let [[first-found-val rest-of-string] (parser/get-first-expr-block (parser/split-unit-str-to-chars long-first-simple-input))]
    (is (= first-found-val "minute"))
    (is (= (first rest-of-string) "*")))
  (let [[first-found-val rest-of-string] (parser/get-first-expr-block (parser/split-unit-str-to-chars mult-first-partial-input))]
    (is (= first-found-val "*"))
    (is (= (first rest-of-string) "m")))
  (let [[first-found-val rest-of-string] (parser/get-first-expr-block (parser/split-unit-str-to-chars div-first-partial-input))]
    (is (= first-found-val "/"))
    (is (= (first rest-of-string) "m")))
  (let [[first-found-val rest-of-string] (parser/get-first-expr-block (parser/split-unit-str-to-chars one-unit-string))]
    (is (= first-found-val "minute"))
    (is (nil? (first rest-of-string)))))  

(deftest it-converts-string-to-vector-of-unit-names-and-operators
  (let [unit-op-vec (parser/get-unit-op-vec short-first-simple-input)]
    (is (= unit-op-vec ["L" "*" "minute"])))
  (let [unit-op-vec (parser/get-unit-op-vec one-unit-string)]
    (is (= unit-op-vec ["minute"])))
  (let [unit-op-vec (parser/get-unit-op-vec "")]
    (is (= unit-op-vec [])))
  (let [unit-op-vec (parser/get-unit-op-vec short-first-complex-input)]
    (is (= unit-op-vec ["ha" "*" "degree" "/" "(" "minute" "*" "tonne" "/" "'" ")"]))))

(deftest it-validates-an-input-str
  (is (parser/string-contents-valid? short-first-complex-input))
  (is (parser/string-contents-valid? long-first-simple-input))
  (is (parser/string-contents-valid? one-unit-string))
  (is (not (parser/string-contents-valid? invalid-plus-input)))
  (is (not (parser/string-contents-valid? invalid-bad-unit-input))))

(deftest it-retrieves-a-multiplier
  (is (= (parser/get-unit-multiplier "ha") 10000.0))
  (is (= (parser/get-unit-multiplier "L") 0.001)))

(deftest it-retrieves-an-si-name
  (is (= (parser/get-unit-multiplier "ha") 10000.0))
  (is (= (parser/get-unit-multiplier "L") 0.001)))

(deftest converts-op-string-to-function
  (is (= ((parser/op-string-to-func "*") 3 2) 6))
  (is (= ((parser/op-string-to-func "/") 8 2) 4)))

(deftest collapses-valid-unit-string-to-multiplier
  (is (= (parser/collapse-uovec-to-multiplier (parser/get-unit-op-vec one-unit-string)) 60.0))
  (let [unit-op-vec (parser/get-unit-op-vec short-first-simple-input)]
    (is (= (parser/collapse-uovec-to-multiplier unit-op-vec) 0.06)))
  (let [unit-op-vec (parser/get-unit-op-vec one-paren-input)]
    (is (= (parser/collapse-uovec-to-multiplier unit-op-vec) 6.0E-6)))
  (let [unit-op-vec (parser/get-unit-op-vec "min/min")]
    (is (= (parser/collapse-uovec-to-multiplier unit-op-vec) 1.0)))
  ; (let [unit-op-vec (parser/get-unit-op-vec "min/(degree*min)*degree")]
  ;   (is (= (parser/collapse-uovec-to-multiplier unit-op-vec) 1.0)))
  ; (let [unit-op-vec (parser/get-unit-op-vec "min/(degree*min/ha)*degree/ha")]
  ;   (is (= (parser/collapse-uovec-to-multiplier unit-op-vec) 1.0)))
  (let [unit-op-vec (parser/get-unit-op-vec "min/(degree/(degree/min*(ha/tonne)))/ha*tonne")]
    (is (= (parser/collapse-uovec-to-multiplier unit-op-vec) 1.0))))


(deftest separates-first-parenthese-block
  (let [unit-op-vec (parser/get-unit-op-vec "(min*deg)/(ha*L)")]
    (let [[first-paren-block after-block] (parser/separate-first-paren-block unit-op-vec)]
      (is (= first-paren-block ["min" "*" "deg"]))
      (is (= after-block ["/" "(" "ha" "*" "L" ")"]))
      ))
  (let [unit-op-vec (parser/get-unit-op-vec "(min*(deg/L))*(ha*L)")]
    (let [[first-paren-block after-block] (parser/separate-first-paren-block unit-op-vec)]
      (is (= first-paren-block ["min" "*" "(" "deg" "/" "L" ")"]))
      (is (= after-block ["*" "(" "ha" "*" "L" ")"]))
      ))
  (let [unit-op-vec (parser/get-unit-op-vec "(min*deg)")]
    (let [[first-paren-block after-block] (parser/separate-first-paren-block unit-op-vec)]
      (is (= first-paren-block ["min" "*" "deg"]))
      (is (= after-block []))
      )))

(deftest constructs-si-name-string
  (let [si-name-str (parser/make-si-unit-name-string short-first-complex-input)]
    (is (= si-name-str "m2*rad/(s*kg/rad)")
    )))

(deftest return-correctly
  (let [result (parser/get-conversion "minute")]
    (is (= (get result :multiplier) 60.0))
    (is (= (get result :name-string) "s"))
    )
  (let [result (parser/get-conversion one-paren-input)]
    (is (= (get result :multiplier) 6.0E-6))
    (is (= (get result :name-string) "m3*(s/m2)"))
    )
  (let [result (parser/get-conversion short-first-complex-input)]
    (is (= (get result :multiplier) 5.076956996445143E-5))
    (is (= (get result :name-string) "m2*rad/(s*kg/rad)"))
    ))

(deftest analyzes-parentheses-in-string
  (is (not (parser/has-balanced-parentheses? "))")))
  (is (parser/has-balanced-parentheses? "()"))
  (is (not (parser/has-balanced-parentheses? "())")))
  (is (not (parser/has-balanced-parentheses? "(minute*degree)/`/hectare)")))
  (is (parser/has-balanced-parentheses? "((minute*degree)/`/hectare)")))

(deftest identifies-valid-in-str
  (is (not (parser/is-valid-input? "minute/mile")))
  (is (parser/is-valid-input? "minute/degree"))
  (is (not (parser/is-valid-input? "(minute*degree)/`/hectare)")))
  (is (parser/is-valid-input? "((minute*degree)/'/hectare)")))

(run-tests)