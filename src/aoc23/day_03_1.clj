(ns aoc23.day-03-1
  (:require [clojure.test :refer [deftest is testing]])
  (:import (java.lang Character)))

(defn scan-line-with-context
  ([line-with-context] (scan-line-with-context {:cache [#{} #{}] :numbers []} line-with-context))
  ([{[previous-line-cache current-line-cache] :cache numbers :numbers} [_previous-line current-line _next-line :as lines]]
   (let [find-number-start (fn [line i]
                             (reduce #(if (Character/isDigit (char (.charAt line %2))) %2 (reduced %1)) i (range (dec i) -1 -1)))
         find-number-end (fn [line i]
                           (inc (reduce #(if (Character/isDigit (char (.charAt line %2))) %2 (reduced %1)) i (range (inc i) (.length line)))))
         update-cache-if-number (fn [line cache i]
                                  (let [char (char (.charAt line i))]
                                    (if (Character/isDigit char)
                                      (let [start (find-number-start line i)
                                            end (find-number-end line i)
                                            number (Integer/parseInt (.substring line start end))]
                                        (conj cache {:start start :number number}))
                                      cache)))
         scan-neighbors (fn [line cache i]
                          (let [js (range (max 0 (dec i)) (min (.length line) (+ i 2)))]
                            (reduce #(update-cache-if-number line %1 %2) cache js)))
         scan-line (fn [caches i]
                     (let [char (char (.charAt current-line i))]
                       (if (or (Character/isDigit char) (= char \.))
                         caches
                         (vec (map #(scan-neighbors %1 %2 i) lines caches)))))
         [new-previous-line-cache new-current-line-cache new-next-line-cache]
         (reduce scan-line [previous-line-cache current-line-cache #{}] (range (.length current-line)))
         new-numbers (vec (concat numbers (map :number new-previous-line-cache)))]
     {:cache [new-current-line-cache new-next-line-cache] :numbers new-numbers})))

(defn solve [lines]
  (let [first-line (first lines)
        blank-line (.repeat "." (.length first-line))
        padded-lines (concat [blank-line] lines [blank-line blank-line])
        line-groups (partition 3 1 padded-lines)
        {part-numbers :numbers} (reduce scan-line-with-context {:cache [#{} #{}] :numbers []} line-groups)]
    (reduce + 0 part-numbers)))

(deftest scan-line-test
  (testing "1"
    (let [{cache :cache numbers :numbers} (scan-line-with-context ["..."
                                                                   "..."
                                                                   "..."])]
      (is (= cache [#{} #{}]))
      (is (= numbers []))))
  (testing "2"
    (let [{cache :cache numbers :numbers} (scan-line-with-context ["467..114.."
                                                                   "...*......"
                                                                   "..35..633."])]
      (is (= cache [#{} #{{:start 2 :number 35}}]))
      (is (= numbers [467]))))
  (testing "3"
    (let [{cache :cache numbers :numbers} (scan-line-with-context {:cache [#{} #{{:start 2 :number 35}}] :numbers [467]}
                                                                  ["...*......"
                                                                   "..35..633."
                                                                   "......#..."])]
      (is (= cache [#{{:start 2 :number 35}} #{}]))
      (is (= numbers [467]))))
  (testing "4"
    (let [{cache :cache numbers :numbers} (scan-line-with-context {:cache [#{{:start 2 :number 35}} #{}] :numbers [467]}
                                                                  ["..35..633."
                                                                   ".....1#..."
                                                                   "617*......"])]
      (is (= cache [#{{:start 5 :number 1}} #{}]))
      (is (= (sort numbers) (sort [467 35 633])))))
  (testing "5"
    (let [{cache :cache numbers :numbers} (scan-line-with-context ["......755."
                                                                   "...$.*...."
                                                                   ".664.598.."])]
      (is (= cache [#{} #{{:start 1 :number 664} {:start 5 :number 598}}]))
      (is (= numbers [755]))))
  (testing "6"
    (let [{cache :cache numbers :numbers} (scan-line-with-context ["1.1"
                                                                   "1/1"
                                                                   "..."])]
      (is (= cache [#{{:start 0 :number 1} {:start 2 :number 1}} #{}]))
      (is (= numbers [1 1]))))
  (testing "7"
    (let [{cache :cache numbers :numbers} (scan-line-with-context {:cache [#{{:start 0 :number 1} {:start 2 :number 1}} #{}] :numbers [1 1]}
                                                                  ["1/1"
                                                                   "..."
                                                                   "..."])]
      (is (= cache [#{} #{}]))
      (is (= numbers [1 1 1 1])))))


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
        {part-numbers :numbers} (reduce scan-line-with-context {:cache [#{} #{}] :numbers []} line-groups)]
    (is (= (sort part-numbers) (sort [467 35 633 617 592 755 664 598])))))
