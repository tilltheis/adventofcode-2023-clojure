(ns aoc23.day-12-2
  (:require [clojure.test :refer [deftest is]]))

(def count-arrangements
  (memoize (fn [spring-states [damaged-group-size & damaged-group-sizes]]
             (cond
               (nil? damaged-group-size) (if (every? #{\. \?} spring-states) 1 0)
               (= (first spring-states) \.) (recur (drop-while #{\.} spring-states) (cons damaged-group-size damaged-group-sizes))
               (empty? spring-states) (if (nil? damaged-group-size) 1 0)
               (#{\# \?} (first spring-states)) (+ (if (and (nth spring-states (dec damaged-group-size) false)
                                                            (every? #{\# \?} (take damaged-group-size spring-states))
                                                            (contains? #{\. \? nil} (nth spring-states damaged-group-size nil)))
                                                     (count-arrangements (drop (inc damaged-group-size) spring-states) damaged-group-sizes)
                                                     0)
                                                   (if (= (first spring-states) \?)
                                                     (count-arrangements (rest spring-states) (cons damaged-group-size damaged-group-sizes))
                                                     0))
               :else (throw (IllegalArgumentException.))))))

(defn parse-condition-record [line]
  (let [[spring-states damaged-group-sizes-str] (.split line " ")
        damaged-group-sizes-strs (.split damaged-group-sizes-str ",")
        damaged-group-sizes (map #(Integer/parseInt %) damaged-group-sizes-strs)
        unfolded-spring-states (apply str (interpose "?" (repeat 5 spring-states)))
        unfolded-damaged-group-sizes (flatten (repeat 5 damaged-group-sizes))]
    [unfolded-spring-states unfolded-damaged-group-sizes]))

(defn solve [lines]
  (reduce + (map #(apply count-arrangements %) (map parse-condition-record lines))))

(deftest count-arrangements-test
  (is (= (apply count-arrangements (parse-condition-record "#.#.### 1,1,3")) 1))
  (is (= (apply count-arrangements (parse-condition-record "???.### 1,1,3")) 1))
  (is (= (apply count-arrangements (parse-condition-record ".??..??...?##. 1,1,3")) 16384))
  (is (= (apply count-arrangements (parse-condition-record "?#?#?#?#?#?#?#? 1,3,1,6")) 1))
  (is (= (apply count-arrangements (parse-condition-record "????.#...#... 4,1,1")) 16))
  (is (= (apply count-arrangements (parse-condition-record "????.######..#####. 1,6,5")) 2500))
  (is (= (apply count-arrangements (parse-condition-record "?###???????? 3,2,1")) 506250)))

(deftest solve-test
  (is (= (solve ["???.### 1,1,3"
                 ".??..??...?##. 1,1,3"
                 "?#?#?#?#?#?#?#? 1,3,1,6"
                 "????.#...#... 4,1,1"
                 "????.######..#####. 1,6,5"
                 "?###???????? 3,2,1"])
         525152)))
