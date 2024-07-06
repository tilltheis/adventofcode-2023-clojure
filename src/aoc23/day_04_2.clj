(ns aoc23.day-04-2
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]))

(defn parse-card [str]
  (let [[card-number-str winning-and-own-numbers-str] (.split str "\\:" 2)
        card-number (Integer/parseInt (re-find #"\d+" card-number-str))
        [winning-numbers-str own-numbers-str] (.split winning-and-own-numbers-str "\\|" 2)
        winning-number-strs (re-seq #"\d+" winning-numbers-str)
        own-number-strs (re-seq #"\d+" own-numbers-str)
        winning-numbers (set (map #(Integer/parseInt %) winning-number-strs))
        own-numbers (set (map #(Integer/parseInt %) own-number-strs))]
    {:card-number card-number :winning-numbers winning-numbers :own-numbers own-numbers}))

(def empty-eval-card-state {:count 0 :copies {}})
(defn eval-card [state card]
  (let [matching-count (count (set/intersection (:winning-numbers card) (:own-numbers card)))
        old-copies (:copies state)
        card-number (:card-number card)
        card-count (inc (get old-copies card-number 0))
        old-copies-without-current (dissoc old-copies card-number)
        delta-copies (zipmap (range (inc card-number) (inc (+ card-number matching-count))) (repeat card-count))
        new-copies (merge-with + old-copies-without-current delta-copies)
        new-count (+ (:count state) card-count)]
    {:count new-count :copies new-copies}))

(defn count-total-cards [cards]
  (:count (reduce eval-card empty-eval-card-state cards)))

(defn solve [lines]
  (count-total-cards (map parse-card lines)))

(deftest parse-card-test
  (is (= (parse-card "Card  99: 12  1 99 |  4 99") {:card-number 99 :winning-numbers #{12 1 99} :own-numbers #{4 99}})))

(deftest eval-card-test
  (testing "no context"
    (is (= (eval-card empty-eval-card-state {:card-number 3 :winning-numbers #{1 2 3} :own-numbers #{4}}) {:count 1 :copies {}}) "no match")
    (is (= (eval-card empty-eval-card-state {:card-number 3 :winning-numbers #{1 2 3} :own-numbers #{3 4}}) {:count 1 :copies {4 1}}) "1 match")
    (is (= (eval-card empty-eval-card-state {:card-number 3 :winning-numbers #{1 2 3} :own-numbers #{2 3 4}}) {:count 1 :copies {4 1, 5 1}}) "2 matches"))
  (testing "with context"
    (is (= (eval-card {:count 2 :copies {4 1, 5 1}} {:card-number 3 :winning-numbers #{1 2 3} :own-numbers #{4}})
           {:count 3 :copies {4 1, 5 1}})
        "no match")
    (is (= (eval-card {:count 2 :copies {4 1, 5 1}} {:card-number 3 :winning-numbers #{1 2 3} :own-numbers #{3 4}})
           {:count 3 :copies {4 2, 5 1}})
        "1 match")
    (is (= (eval-card {:count 2 :copies {4 1, 5 1}} {:card-number 3 :winning-numbers #{1 2 3} :own-numbers #{2 3 4}})
           {:count 3 :copies {4 2, 5 2}})
        "2 matches"))
  (testing "with own context"
    (is (= (eval-card {:count 2 :copies {3 1, 4 1, 5 1}} {:card-number 3 :winning-numbers #{1 2 3} :own-numbers #{2 3 4}})
           {:count 4 :copies {4 3, 5 3}})
        "2 matches, accept and drop own context")))

(deftest solve-test
  (is (= (solve ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                 "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                 "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                 "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                 "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                 "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]) 30)))
