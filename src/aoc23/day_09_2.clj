(ns aoc23.day-09-2
  (:require [clojure.test :refer [deftest is]]))

(defn previous-number [xs]
  (if (every? zero? xs)
    0
    (- (first xs) (previous-number (vec (map #(- %2 %1) xs (rest xs)))))))

(defn parse-sequence [line]
  (map #(Integer/parseInt %) (.split line " ")))

(defn solve [lines]
  (let [sequences (map parse-sequence lines)
        previous-numbers (map previous-number sequences)]
    (reduce + previous-numbers)))

(deftest parse-sequence-test
  (is (= (parse-sequence "0 3 6 9 12 15") [0 3 6 9 12 15]))
  (is (= (parse-sequence "1 3 -6 10 -15 21") [1 3 -6 10 -15 21])))

(deftest previous-number-test
  (is (= (previous-number [0 3 6 9 12 15]) -3))
  (is (= (previous-number [1 3 6 10 15 21]) 0))
  (is (= (previous-number [10 13 16 21 30 45]) 5)))

(deftest solve-test
  (is (= (solve ["0 3 6 9 12 15"
                 "1 3 6 10 15 21"
                 "10 13 16 21 30 45"])
         2)))