(ns aoc23.day-03-2
  (:require [clojure.test :refer [deftest is testing]])
  (:import (java.lang Character)))

(defn- find-number-start [line i]
  (reduce #(if (Character/isDigit (char (.charAt line %2))) %2 (reduced %1)) i (range (dec i) -1 -1)))

(defn- find-number-end [line i]
  (inc (reduce #(if (Character/isDigit (char (.charAt line %2))) %2 (reduced %1)) i (range (inc i) (.length line)))))

(defn- find-number [line i]
  (let [char (char (.charAt line i))]
    (when (Character/isDigit char)
      (let [start (find-number-start line i)
            end (find-number-end line i)
            number (Integer/parseInt (.substring line start end))]
        {:start start :number number}))))

(defn- scan-neighbors [line i]
  (let [js (range (max 0 (dec i)) (min (.length line) (+ i 2)))]
    (map :number (set (keep #(find-number line %) js)))))

(defn- scan-line [[_previous-line current-line _next-line :as lines] i]
  (let [char (char (.charAt current-line i))]
    (when (= char \*)
      (let [numbers (mapcat #(scan-neighbors % i) lines)]
        (when (= (count numbers) 2)
          numbers)))))

(defn scan-line-with-context
  ([[_previous-line current-line _next-line :as lines]]
   (keep #(scan-line lines %) (range (.length current-line)))))

(defn solve [lines]
  (let [first-line (first lines)
        blank-line (.repeat "." (.length first-line))
        padded-lines (concat [blank-line] lines [blank-line blank-line])
        line-groups (partition 3 1 padded-lines)
        gears (mapcat scan-line-with-context line-groups)
        gear-ratio-sum (reduce + 0 (map (fn [[x y]] (* x y)) gears))]
    gear-ratio-sum))

(deftest find-number-test
  (is (= (find-number "1.1" 0) {:start 0 :number 1}) "number start")
  (is (nil? (find-number "1.1" 1)) "no number middle")
  (is (= (find-number "1.1" 2) {:start 2 :number 1}) "number end"))

(deftest scan-neighbors-test
  (is (empty? (scan-neighbors "..." 1)) "no number")
  (is (= (scan-neighbors "123" 1) [123]) "one number everywhere")
  (is (= (scan-neighbors "1.." 1) [1]) "one number left")
  (is (= (scan-neighbors "1.1" 1) [1 1]) "two numbers, left and right"))

(deftest scan-line-with-context-test
  (testing "1"
    (let [gears (scan-line-with-context ["..."
                                         "..."
                                         "..."])]
      (is (= gears []))))
  (testing "2"
    (let [gears (scan-line-with-context ["467..114.."
                                         "...*......"
                                         "..35..633."])]
      (is (= gears [[467 35]]))))
  (testing "3"
    (let [gears (scan-line-with-context ["...*......"
                                         "..35..633."
                                         "......#..."])]
      (is (= gears []))))
  (testing "4"
    (let [gears (scan-line-with-context ["...1"
                                         "1*1*"
                                         "...."])]
      (is (= gears [[1 1] [1 1]]))))
  (testing "5"
    (let [gears (scan-line-with-context ["1..1"
                                         "1*1*"
                                         "...."])]
      (is (= gears [[1 1]]))))
  (testing "6"
    (let [gears (scan-line-with-context ["1.1"
                                         ".*."
                                         "..."])]
      (is (= gears [[1 1]]))))
  (testing "7"
    (let [gears (scan-line-with-context ["111"
                                         ".*."
                                         "..."])]
      (is (= gears [])))))


(deftest full-example-test
  (let [padded-lines [".........."
                      "467..114.."
                      "...*......"
                      "..35..633."
                      "......#..."
                      "617*......"
                      ".....+.58."
                      "..592....."
                      "......755."
                      "...$.*...."
                      ".664.598.."
                      ".........."
                      ".........."]
        line-groups (partition 3 1 padded-lines)
        gears (mapcat scan-line-with-context line-groups)
        gear-ratio-sum (reduce + 0 (map (fn [[x y]] (* x y)) gears))]
    (is (= gears [[467 35] [755 598]]))
    (is (= gear-ratio-sum 467835))))
