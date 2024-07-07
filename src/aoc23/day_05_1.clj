(ns aoc23.day-05-1
  (:require [clojure.test :refer [deftest is]]))

(defn mapper [ranges]
  (let [ranges (map (fn [[dst-range-start src-range-start range-len]]
                      {:from   src-range-start,
                       :until  (+ src-range-start range-len),
                       :offset (- dst-range-start src-range-start)})
                    ranges)]
    (fn [x] (let [in-range? (fn [r] (and (>= x (:from r)) (< x (:until r))))
                  range-mapped-value (some #(when (in-range? %) (+ x (:offset %))) ranges)]
              (or range-mapped-value x)))))

(defn parse-all-numbers [line]
  (map #(Long/parseLong %) (re-seq #"\d+" line)))

(defn solve [lines]
  (let [seeds (parse-all-numbers (first lines))
        mapper-lines-strs (map rest (filter #(not= [""] %) (partition-by empty? (rest lines))))
        mapper-lines (map #(map parse-all-numbers %) mapper-lines-strs)
        mappers (map mapper mapper-lines)]
    (apply min (reduce (fn [xs f] (map f xs)) seeds mappers))))

(deftest mapper-test
  (let [f (mapper [[50 98 2]
                   [52 50 48]])]
    (is (= (f 0) 0))
    (is (= (f 1) 1))
    (is (= (f 48) 48))
    (is (= (f 49) 49))
    (is (= (f 50) 52))
    (is (= (f 51) 53))
    (is (= (f 96) 98))
    (is (= (f 97) 99))
    (is (= (f 98) 50))
    (is (= (f 99) 51))))

(deftest solve-test
  (is (= (solve ["seeds: 79 14 55 13"
                 ""
                 "seed-to-soil map:"
                 "50 98 2"
                 "52 50 48"
                 ""
                 "soil-to-fertilizer map:"
                 "0 15 37"
                 "37 52 2"
                 "39 0 15"
                 ""
                 "fertilizer-to-water map:"
                 "49 53 8"
                 "0 11 42"
                 "42 0 7"
                 "57 7 4"
                 ""
                 "water-to-light map:"
                 "88 18 7"
                 "18 25 70"
                 ""
                 "light-to-temperature map:"
                 "45 77 23"
                 "81 45 19"
                 "68 64 13"
                 ""
                 "temperature-to-humidity map:"
                 "0 69 1"
                 "1 0 69"
                 ""
                 "humidity-to-location map:"
                 "60 56 37"
                 "56 93 4"])
         35)))
