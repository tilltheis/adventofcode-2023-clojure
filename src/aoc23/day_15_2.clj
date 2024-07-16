(ns aoc23.day-15-2
  (:require [clojure.test :refer [deftest is]]))

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
  (case operation
    :add (update boxes box-number (fn [lenses]
                                    (let [index (reduce-kv (fn [_ i lens] (when (= (:label lens) label) (reduced i))) nil lenses)]
                                      (if index (vec (update lenses index (fn [x] (merge x {:focal-length focal-length}))))
                                                (vec (conj lenses {:label label :focal-length focal-length}))))))
    :remove (update boxes box-number (fn [lenses] (vec (filter #(not= (:label %) label) lenses))))))

(defn solve [lines]
  (let [empty-boxes (vec (repeat 256 []))
        instructions (map parse-instruction (.split (first lines) ","))
        filled-boxes (reduce handle-instruction empty-boxes instructions)
        focusing-factors (mapcat (fn [i lenses] (map #(* i %1 (:focal-length %2))
                                                     (iterate inc 1)
                                                     lenses))
                                 (iterate inc 1)
                                 filled-boxes)]
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
  (is (= (handle-instruction [[] []]
                             {:box-number 0 :label "rn" :operation :add :focal-length 1})
         [[{:label "rn" :focal-length 1}] []]))
  (is (= (handle-instruction [[{:label "rn" :focal-length 1}] []]
                             {:box-number 0 :label "cm" :operation :remove})
         [[{:label "rn" :focal-length 1}] []]))
  (is (= (handle-instruction [[{:label "rn" :focal-length 1}] []]
                             {:box-number 1 :label "gp" :operation :add :focal-length 3})
         [[{:label "rn" :focal-length 1}] [{:label "gp" :focal-length 3}]]))
  (is (= (handle-instruction [[{:label "rn" :focal-length 1}] [{:label "gp" :focal-length 3}]]
                             {:box-number 0 :label "cm" :operation :add :focal-length 2})
         [[{:label "rn" :focal-length 1} {:label "cm" :focal-length 2}] [{:label "gp" :focal-length 3}]]))
  (is (= (handle-instruction [[{:label "rn" :focal-length 1} {:label "cm" :focal-length 2}] [{:label "gp" :focal-length 3}]]
                             {:box-number 1 :label "gp" :operation :remove})
         [[{:label "rn" :focal-length 1} {:label "cm" :focal-length 2}] []]))
  (is (= (handle-instruction [[{:label "rn" :focal-length 1} {:label "cm" :focal-length 2}] []]
                             {:box-number 0 :label "rn" :operation :add :focal-length 9})
         [[{:label "rn" :focal-length 9} {:label "cm" :focal-length 2}] []])))

(deftest solve-test
  (is (= (solve ["rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"]) 145)))
