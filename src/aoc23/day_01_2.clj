(ns aoc23.day-01-2
  (:require [clojure.test :refer [deftest is]]))

(def ^:private ^:const digits
  {"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9
   "0"    0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9})

(defn first-digit [str]
  (let [prefixes (map #(.substring str %) (range (.length str)))
        all-matches (mapcat (fn [prefix] (map (fn [[k v]] (when (.startsWith prefix k) v)) digits)) prefixes)
        first-match (first (filter some? all-matches))]
    first-match))

(defn last-digit [str]
  (let [postfixes (map #(.substring str 0 %) (range (.length str) 0 -1))
        all-matches (mapcat (fn [postfix] (map (fn [[k v]] (when (.endsWith postfix k) v)) digits)) postfixes)
        first-match (first (filter some? all-matches))]
    first-match))

(defn calibration-value [fst-digit lst-digit] (+ (* 10 fst-digit) lst-digit))

(defn solve [lines]
  (reduce + 0 (map (fn [line] (calibration-value (first-digit line) (last-digit line))) lines)))

(deftest first-digit-test
  (is (= (first-digit "ab123cd") 1))
  (is (= (first-digit "9ab123cd") 9))
  (is (= (first-digit "abone23cd") 1))
  (is (= (first-digit "nineab123cd") 9))
  (is (= (first-digit "abc") nil)))

(deftest last-digit-test
  (is (= (last-digit "ab123cd") 3))
  (is (= (last-digit "ab123cd9") 9))
  (is (= (last-digit "ab12threecd") 3))
  (is (= (last-digit "ab123cdnine") 9))
  (is (= (last-digit "abc") nil)))

(deftest calibration-value-test
  (is (= (calibration-value 1 7) 17)))

(deftest combination-test
  (is (= (calibration-value (first-digit "123") (last-digit "123")) 13)))
