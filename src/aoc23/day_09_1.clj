(ns aoc23.day-09-1
  (:require [clojure.test :refer [deftest is]]))

(defn next-number [xs]
  (if (every? zero? xs)
    0
    (+ (last xs) (next-number (map #(- %2 %1) xs (rest xs))))))

(defn parse-sequence [line]
  (map #(Integer/parseInt %) (.split line " ")))

(defn solve [lines]
  (let [sequences (map parse-sequence lines)
        next-numbers (map next-number sequences)]
    (reduce + next-numbers)))

(deftest parse-sequence-test
  (is (= (parse-sequence "0 3 6 9 12 15") [0 3 6 9 12 15]))
  (is (= (parse-sequence "1 3 -6 10 -15 21") [1 3 -6 10 -15 21])))

(deftest next-number-test
  (is (= (next-number [0 3 6 9 12 15]) 18))
  (is (= (next-number [1 3 6 10 15 21]) 28))
  (is (= (next-number [10 13 16 21 30 45]) 68)))

(deftest solve-test
  (is (= (solve ["0 3 6 9 12 15"
                 "1 3 6 10 15 21"
                 "10 13 16 21 30 45"])
         114)))