(ns aoc23.day-07-1
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
  (let [sorted-frequency-counts (sort #(compare %2 %1) (vals (frequencies hand)))]
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
    \J 11
    \Q 12
    \K 13
    \A 14
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
  (is (= (hand-type "AA8AA") 5))
  (is (= (hand-type "23332") 4))
  (is (= (hand-type "TTT98") 3))
  (is (= (hand-type "23423") 2))
  (is (= (hand-type "A23A4") 1))
  (is (= (hand-type "23456") 0)))

(deftest compare-hands-test
  (is (zero? (compare-hands "AAAAA" "AAAAA")) "AAAAA vs AAAAA")
  (is (pos? (compare-hands "AAAAA" "AA8AA")) "AAAAA vs AA8AA")
  (is (pos? (compare-hands "AA8AA" "23332")) "AA8AA vs 23332")
  (is (pos? (compare-hands "23332" "TTT98")) "23332 vs TTT98")
  (is (pos? (compare-hands "TTT98" "23423")) "TTT98 vs 23423")
  (is (pos? (compare-hands "23423" "A23A4")) "23423 vs A23A4")
  (is (pos? (compare-hands "A23A4" "23456")) "A23A4 vs 23456")
  (is (neg? (compare-hands "2AAAA" "33332")) "2AAAA vs 33332")
  (is (pos? (compare-hands "77999" "77788")) "77999 vs 77788"))

(deftest ranked-game-winnings-test
  (is (= (ranked-game-winnings [{:bid 1} {:bid 5} {:bid 4}]) [1 10 12])))

(deftest solve-test
  (is (= (solve ["32T3K 765"
                 "T55J5 684"
                 "KK677 28"
                 "KTJJT 220"
                 "QQQJA 483"])
         6440)))
