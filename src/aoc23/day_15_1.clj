(ns aoc23.day-15-1
  (:require [clojure.test :refer [deftest is]]))

(defn hash-string [string]
  (reduce (fn [hash char] (-> hash
                              (+ (int char))
                              (* 17)
                              (mod 256))) 0 string))

(defn solve [lines]
  (reduce + (map hash-string (.split (first lines) ","))))

(deftest hash-string-test
  (is (= (hash-string "HASH") 52))
  (is (= (hash-string "rn=1") 30))
  (is (= (hash-string "cm-") 253)))

(deftest solve-test
  (is (= (solve ["rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"]) 1320)))
