; same as day 15 part 2 but using java.util LinkedHashMap instead of native clojure types
(ns aoc23.day-15-3
  (:require [clojure.test :refer [deftest is]])
  (:import (java.util LinkedHashMap LinkedHashMap$Entry)))

(defn hash-string [string]
  (reduce (fn [hash char] (-> hash
                              (+ (int char))
                              (* 17)
                              (mod 256))) 0 string))

(defn parse-instruction [instruction-str]
  (let [[label-str focal-len-str] (.split instruction-str "-|=")]
    (merge {:box-number (hash-string label-str)
            :label      label-str
            :operation  (if focal-len-str :add :remove)}
           (when focal-len-str {:focal-length (Integer/parseInt focal-len-str)}))))

(defn handle-instruction [boxes {box-number :box-number label :label operation :operation focal-length :focal-length}]
  (let [^LinkedHashMap lenses (nth boxes box-number)]
    (case operation
      :add (.put lenses label focal-length)
      :remove (.remove lenses label))))

(defn solve [lines]
  (let [boxes (vec (repeatedly 256 #(LinkedHashMap.)))
        instructions (map parse-instruction (.split (first lines) ","))
        _ (run! #(handle-instruction boxes %) instructions)
        focusing-factors (mapcat (fn [i lenses] (map #(* i %1 (.getValue ^LinkedHashMap$Entry %2))
                                                     (iterate inc 1)
                                                     lenses))
                                 (iterate inc 1)
                                 boxes)]
    (reduce + focusing-factors)))

(deftest hash-string-test
  (is (= (hash-string "HASH") 52))
  (is (= (hash-string "rn=1") 30))
  (is (= (hash-string "cm-") 253)))

(deftest parse-instruction-test
  (is (= (parse-instruction "rn=1") {:box-number 0 :label "rn" :operation :add :focal-length 1}))
  (is (= (parse-instruction "cm-") {:box-number 0 :label "cm" :operation :remove}))
  (is (= (parse-instruction "qp=3") {:box-number 1 :label "qp" :operation :add :focal-length 3})))

(deftest handle-instruction-test
  (let [box1 (LinkedHashMap.)
        box2 (LinkedHashMap.)]
    (handle-instruction [box1 box2]
                        {:box-number 0 :label "rn" :operation :add :focal-length 1})
    (is (and (= box1 (doto (LinkedHashMap.) (.put "rn" 1)))
             (= box2 (LinkedHashMap.)))))
  (let [box1 (doto (LinkedHashMap.) (.put "rn" 1))
        box2 (LinkedHashMap.)]
    (handle-instruction [box1 box2]
                        {:box-number 0 :label "cm" :operation :remove})
    (is (and (= box1 (doto (LinkedHashMap.) (.put "rn" 1)))
             (= box2 (LinkedHashMap.)))))
  (let [box1 (doto (LinkedHashMap.) (.put "rn" 1))
        box2 (LinkedHashMap.)]
    (handle-instruction [box1 box2]
                        {:box-number 1 :label "gp" :operation :add :focal-length 3})
    (is (and (= box1 (doto (LinkedHashMap.) (.put "rn" 1)))
             (= box2 (doto (LinkedHashMap.) (.put "gp" 3))))))
  (let [box1 (doto (LinkedHashMap.) (.put "rn" 1))
        box2 (doto (LinkedHashMap.) (.put "gp" 3))]
    (handle-instruction [box1 box2]
                        {:box-number 0 :label "cm" :operation :add :focal-length 2})
    (is (and (= box1 (doto (LinkedHashMap.) (.put "rn" 1) (.put "cm" 2)))
             (= box2 (doto (LinkedHashMap.) (.put "gp" 3))))))
  (let [box1 (doto (LinkedHashMap.) (.put "rn" 1) (.put "cm" 2))
        box2 (doto (LinkedHashMap.) (.put "gp" 3))]
    (handle-instruction [box1 box2]
                        {:box-number 1 :label "gp" :operation :remove})
    (is (and (= box1 (doto (LinkedHashMap.) (.put "rn" 1) (.put "cm" 2)))
             (= box2 (LinkedHashMap.)))))
  (let [box1 (doto (LinkedHashMap.) (.put "rn" 1) (.put "cm" 2))
        box2 (LinkedHashMap.)]
    (handle-instruction [box1 box2]
                        {:box-number 0 :label "rn" :operation :add :focal-length 9})
    (is (and (= box1 (doto (LinkedHashMap.) (.put "rn" 9) (.put "cm" 2)))
             (= box2 (LinkedHashMap.))))))

(deftest solve-test
  (is (= (solve ["rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"]) 145)))
