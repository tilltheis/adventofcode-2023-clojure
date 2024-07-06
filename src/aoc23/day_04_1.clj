(ns aoc23.day-04-1
  (:require [clojure.test :refer [deftest is]]
            [clojure.set :as set]
            [clojure.math :as math]))

(defn parse-card [str]
  (let [[_ number-parts-str] (.split str "\\:" 2)
        [winning-numbers-str own-numbers-str] (.split number-parts-str "\\|" 2)
        winning-number-strs (re-seq #"\d+" winning-numbers-str)
        own-number-strs (re-seq #"\d+" own-numbers-str)
        winning-numbers (set (map #(Integer/parseInt %) winning-number-strs))
        own-numbers (set (map #(Integer/parseInt %) own-number-strs))]
    {:winning-numbers winning-numbers :own-numbers own-numbers}))

(defn score-card [card]
  (let [matching-numbers (set/intersection (:winning-numbers card) (:own-numbers card))
        matching-count (count matching-numbers)]
    (if (pos? matching-count) (int (math/pow 2 (dec matching-count))) 0)))

(defn solve [lines]
  (reduce + 0 (map #(-> % parse-card score-card) lines)))

(deftest parse-card-test
  (is (= (parse-card "Card  99: 12  1 99 |  4 99") {:winning-numbers #{12 1 99} :own-numbers #{4 99}})))

(deftest score-card-test
  (is (= (score-card {:winning-numbers #{12 1 99} :own-numbers #{4 77}}) 0))
  (is (= (score-card {:winning-numbers #{12 1 99} :own-numbers #{4 99}}) 1))
  (is (= (score-card {:winning-numbers #{12 1 99} :own-numbers #{4 99 12}}) 2))
  (is (= (score-card {:winning-numbers #{12 1 99} :own-numbers #{1 99 12}}) 4)))
