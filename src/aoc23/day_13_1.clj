(ns aoc23.day-13-1
  (:require [clojure.test :refer [deftest is]]))

(defn find-horizontal-reflection [grid]
  (let [grid (vec grid)
        horizon (for [i (range 1 (count grid))
                      :let [len (min i (- (count grid) i))
                            left (take len (drop (- i len) grid))
                            right (reverse (take len (drop i grid)))]
                      :when (= left right)]
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
         4))
  (is (= (find-horizontal-reflection ["#.##..##."
                                      "..#.##.#."
                                      "##......#"
                                      "##......#"
                                      "..#.##.#."
                                      "..##..##."
                                      "#.#.##.#."])
         nil)))

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
         5)))

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
         405)))
