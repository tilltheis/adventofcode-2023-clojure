(ns aoc23.day-05-2
  (:require [clojure.test :refer [deftest is]]))

(defn mapper
  "returned function receives a range and returns many ranges"
  [offset-table]
  (let [offset-ranges (map (fn [[dst-range-start src-range-start range-len]]
                             {:from   src-range-start,
                              :until  (+ src-range-start range-len),
                              :offset (- dst-range-start src-range-start)})
                           offset-table)
        sorted-offset-ranges (sort-by :from offset-ranges)]
    (fn f [x]
      (let [starts-within-x? (fn [r] (and (> (:from r) (:from x)) (< (:from r) (:until x))))
            ends-within-x? (fn [r] (and (> (:until r) (:from x)) (< (:until r) (:until x))))
            contains-x? (fn [r] (and (<= (:from r) (:from x)) (>= (:until r) (:until x))))
            overlaps-x? (fn [r] (or (starts-within-x? r) (ends-within-x? r) (contains-x? r)))
            r (first (filter overlaps-x? sorted-offset-ranges))
            unmapped-range (fn [from until] {:from from :until until})
            mapped-range (fn [from until] (unmapped-range (+ from (:offset r)) (+ until (:offset r))))]
        (cond
          (nil? r) (list x)
          (contains-x? r) (list (mapped-range (:from x) (:until x)))
          (and (not (starts-within-x? r))
               (ends-within-x? r)) (conj (f (unmapped-range (:until r) (:until x)))
                                         (mapped-range (:from x) (:until r)))
          (and (starts-within-x? r)
               (ends-within-x? r)) (conj (f (unmapped-range (:from r) (:until x)))
                                         (unmapped-range (:from x) (:from r)))
          (and (starts-within-x? r)
               (not (ends-within-x? r))) (list (unmapped-range (:from x) (:from r))
                                               (mapped-range (:from r) (:until x)))
          :else (throw (IllegalStateException. "unreachable condition")))))))

(defn parse-all-numbers [line]
  (map #(Long/parseLong %) (re-seq #"\d+" line)))

(defn solve [lines]
  (let [seed-numbers (parse-all-numbers (first lines))
        seeds (map (fn [[from len]] {:from from :until (+ from len)}) (partition 2 seed-numbers))
        mapper-lines-strs (map rest (filter #(not= [""] %) (partition-by empty? (rest lines))))
        mapper-lines (map #(map parse-all-numbers %) mapper-lines-strs)
        mappers (map mapper mapper-lines)
        final-values (reduce (fn [xs f] (mapcat f xs)) seeds mappers)]
    (apply min (map :from final-values))))

(deftest mapper-test
  (let [f (mapper [[50 98 2]
                   [52 50 48]])]
    (is (= (f {:from 0 :until 4}) [{:from 0 :until 4}]))
    (is (= (f {:from 1 :until 5}) [{:from 1 :until 5}]))
    (is (= (f {:from 48 :until 52}) [{:from 48 :until 50} {:from 52 :until 54}]))
    (is (= (f {:from 49 :until 53}) [{:from 49 :until 50} {:from 52 :until 55}]))
    (is (= (f {:from 50 :until 54}) [{:from 52 :until 56}]))
    (is (= (f {:from 51 :until 55}) [{:from 53 :until 57}]))
    (is (= (f {:from 96 :until 100}) [{:from 98 :until 100} {:from 50 :until 52}]))
    (is (= (f {:from 97 :until 101}) [{:from 99 :until 100} {:from 50 :until 52} {:from 100 :until 101}]))
    (is (= (f {:from 98 :until 102}) [{:from 50 :until 52} {:from 100 :until 102}]))
    (is (= (f {:from 99 :until 103}) [{:from 51 :until 52} {:from 100 :until 103}]))
    (is (= (f {:from 100 :until 104}) [{:from 100 :until 104}]))))

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
         46)))
