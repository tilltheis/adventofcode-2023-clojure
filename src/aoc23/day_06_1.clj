(ns aoc23.day-06-1
  (:require [clojure.math :as math]
            [clojure.test :refer [deftest is]]))

(defn find-thresholds [time distance]
  ; apply p-q formula after setting `x * (time - x) = distance`
  (let [p-half (/ time 2)
        plus-minus (math/sqrt (- (* p-half p-half) distance))]
    [(- p-half plus-minus) (+ p-half plus-minus)]))

(defn count-possibilities [time distance]
  (let [[x1 x2] (find-thresholds time distance)
        ; if x1 or x2 are whole numbers we only match the record
        ; to beat the record we must reach better numbers
        x1 (if (= (double (int x1)) x1) (inc x1) x1)
        x2 (if (= (double (int x2)) x2) (dec x2) x2)]
    (inc (int (- (math/floor x2) (math/ceil x1))))))

(defn parse-all-numbers [line]
  (map #(Long/parseLong %) (re-seq #"\d+" line)))

(defn solve [[line1 line2]]
  (let [times (parse-all-numbers line1)
        distances (parse-all-numbers line2)
        possibility-counts (map count-possibilities times distances)]
    (reduce * 1 possibility-counts)))

(defn rel= [x y] (< (abs (- x y)) 0.01))

(deftest find-thresholds-test
  (let [[x1 x2] (find-thresholds 7 9)]
    (is (rel= x1 1.7))
    (is (rel= x2 5.3))))

(deftest count-possibilities-test
  (is (= (count-possibilities 7 9) 4))
  (is (= (count-possibilities 15 40) 8))
  (is (= (count-possibilities 30 200) 9)))

(deftest solve-test
  (is (= (solve ["Time:      7  15   30"
                 "Distance:  9  40  200"])
         288)))
