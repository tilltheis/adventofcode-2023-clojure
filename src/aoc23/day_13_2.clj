(ns aoc23.day-13-2
  (:require [clojure.test :refer [deftest is]]))

(defn- count-different-elements [xs ys]
  (reduce + (map #(if (= %1 %2) 0 1) xs ys)))

(defn find-horizontal-reflection [grid]
  (let [grid (vec grid)
        horizon (for [i (range 1 (count grid))
                      :let [len (min i (- (count grid) i))
                            left (take len (drop (- i len) grid))
                            right (reverse (take len (drop i grid)))
                            diff (reduce + (map count-different-elements left right))]
                      :when (= diff 1)]
                  i)]
    (first horizon)))

(defn find-vertical-reflection [grid]
  (find-horizontal-reflection (apply mapv vector grid)))

(defn solve [lines]
  (let [grids (filter #(not= [""] %) (partition-by empty? lines))
        summands (map #(if-let [x (find-horizontal-reflection %)] (* 100 x)
                                                                  (find-vertical-reflection %)) grids)]
    (reduce + summands)))

(deftest find-horizontal-reflection-test
  (is (= (find-horizontal-reflection ["#...##..#"
                                      "#....#..#"
                                      "..##..###"
                                      "#####.##."
                                      "#####.##."
                                      "..##..###"
                                      "#....#..#"])
         1))
  (is (= (find-horizontal-reflection ["#.##..##."
                                      "..#.##.#."
                                      "##......#"
                                      "##......#"
                                      "..#.##.#."
                                      "..##..##."
                                      "#.#.##.#."])
         3)))

(deftest find-vertical-reflection-test
  (is (= (find-vertical-reflection ["#...##..#"
                                    "#....#..#"
                                    "..##..###"
                                    "#####.##."
                                    "#####.##."
                                    "..##..###"
                                    "#....#..#"])
         nil))
  (is (= (find-vertical-reflection ["#.##..##."
                                    "..#.##.#."
                                    "##......#"
                                    "##......#"
                                    "..#.##.#."
                                    "..##..##."
                                    "#.#.##.#."])
         nil)))

(deftest solve-test
  (is (= (solve ["#.##..##."
                 "..#.##.#."
                 "##......#"
                 "##......#"
                 "..#.##.#."
                 "..##..##."
                 "#.#.##.#."
                 ""
                 "#...##..#"
                 "#....#..#"
                 "..##..###"
                 "#####.##."
                 "#####.##."
                 "..##..###"
                 "#....#..#"])
         400)))
