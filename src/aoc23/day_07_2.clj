(ns aoc23.day-07-2
  (:require [clojure.test :refer [deftest is]]))

(defn parse-game-line [line]
  (let [[hand bid-str] (.split line " ")
        bid (Integer/parseInt bid-str)]
    {:hand hand :bid bid}))

(defn parse-game [lines]
  (map parse-game-line lines))

(defn starts-with? [prefix coll]
  (every? identity (map = prefix coll)))

(defn hand-type [hand]
  (let [freqs (frequencies hand)
        joker-count (get freqs \J 0)
        regular-sorted-counts (sort #(compare %2 %1) (vals (dissoc freqs \J)))
        [head & tail] regular-sorted-counts
        sorted-frequency-counts (cons (+ (or head 0) joker-count) tail)]
    (condp starts-with? sorted-frequency-counts
      [5] 6
      [4] 5
      [3 2] 4
      [3] 3
      [2 2] 2
      [2] 1
      0)))

(defn card-value [card]
  (case card
    \T 10
    \J 1
    \Q 11
    \K 12
    \A 13
    (- (int card) (int \0))))

(defn compare-hands [hand1 hand2]
  (let [c (compare (hand-type hand1) (hand-type hand2))]
    (if (not= c 0)
      c
      (compare (vec (map card-value hand1)) (vec (map card-value hand2))))))

(defn ranked-game-winnings
  "ranked game is sorted from lowest to highest hand"
  ([ranked-game]
   (map * (iterate inc 1) (map :bid ranked-game))))

(defn solve [lines]
  (let [unranked-game (parse-game lines)
        ranked-game (sort-by :hand compare-hands unranked-game)
        winnings (ranked-game-winnings ranked-game)]
    (reduce + winnings)))

(deftest parse-game-test
  (is (= (parse-game ["12345 123" "AAAAA 99"]) [{:hand "12345" :bid 123} {:hand "AAAAA" :bid 99}])))

(deftest hand-type-test
  (is (= (hand-type "AAAAA") 6))
  (is (= (hand-type "JJJJJ") 6))
  (is (= (hand-type "AA8AA") 5))
  (is (= (hand-type "23332") 4))
  (is (= (hand-type "TTT98") 3))
  (is (= (hand-type "23423") 2))
  (is (= (hand-type "A23A4") 1))
  (is (= (hand-type "23456") 0)))

(deftest compare-hands-test
  (is (= (sort compare-hands ["32T3K"
                              "T55J5"
                              "KK677"
                              "KTJJT"
                              "QQQJA"])
         ["32T3K"
          "KK677"
          "T55J5"
          "QQQJA"
          "KTJJT"])))

(deftest ranked-game-winnings-test
  (is (= (ranked-game-winnings [{:bid 1} {:bid 5} {:bid 4}]) [1 10 12])))

(deftest solve-test
  (is (= (solve ["32T3K 765"
                 "T55J5 684"
                 "KK677 28"
                 "KTJJT 220"
                 "QQQJA 483"])
         5905)))
