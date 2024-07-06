(ns aoc23.day-01-1
  (:require [clojure.test :refer [deftest is]]))

(defn first-digit [str] (first (map #(- % (int \0)) (filter #(and (>= % (int \0)) (<= % (int \9))) (map int (seq str))))))
(defn last-digit [str] (first-digit (seq (map #(.charAt str %) (map #(- (- (.length str) 1) %) (range (.length str)))))))

(defn calibration-value [fst-digit lst-digit] (+ (* 10 fst-digit) lst-digit))

(defn solve [lines]
  (reduce + 0 (map (fn [line] (calibration-value (first-digit line) (last-digit line))) lines)))

(deftest first-digit-test
  (is (= (first-digit "ab123cd") 1))
  (is (= (first-digit "9ab123cd") 9))
  (is (= (first-digit "abc") nil)))

(deftest last-digit-test
  (is (= (last-digit "ab123cd") 3))
  (is (= (last-digit "ab123cd9") 9))
  (is (= (last-digit "abc") nil)))

(deftest calibration-value-test
  (is (= (calibration-value 1 7) 17)))

(deftest combination-test
  (is (= (calibration-value (first-digit "123") (last-digit "123")) 13)))
