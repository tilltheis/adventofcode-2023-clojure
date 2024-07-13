(ns aoc23.day-11-1
  (:require [clojure.test :refer [deftest is]]))

(defn solve [lines]
  (let [lines (vec lines)
        empty-row-indexes (keep-indexed (fn [i l] (when (every? #(= % \.) l) i)) lines)
        empty-col-indexes (keep-indexed (fn [i l] (when (every? #(= % \.) l) i)) (apply mapv vector lines))
        galaxy-locations (vec (filter some? (for [row (range (count lines))
                                                  col (range (count (first lines)))]
                                              (when (= (get-in lines [row col]) \#) [row col]))))
        galaxy-combinations (for [i (range (count galaxy-locations))
                                  j (range (inc i) (count galaxy-locations))]
                              [(galaxy-locations i) (galaxy-locations j)])
        measure-distance (fn [[[row1 col1] [row2 col2]]]
                           (let [[col1 col2] (if (<= col1 col2) [col1 col2] [col2 col1])
                                 naive-dist (+ (- row2 row1) (abs (- col2 col1)))
                                 add-row-dists (count (take-while #(< % row2) (drop-while #(< % row1) empty-row-indexes)))
                                 add-col-dists (count (take-while #(< % col2) (drop-while #(< % col1) empty-col-indexes)))]
                             (+ naive-dist add-row-dists add-col-dists)))
        galaxy-distances (map measure-distance galaxy-combinations)]
    (reduce + galaxy-distances)))

(deftest solve-test
  (is (= (solve ["...#......"
                 ".......#.."
                 "#........."
                 ".........."
                 "......#..."
                 ".#........"
                 ".........#"
                 ".........."
                 ".......#.."
                 "#...#....."])
         374)))
