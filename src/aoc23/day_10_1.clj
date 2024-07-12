(ns aoc23.day-10-1
  (:require [clojure.test :refer [deftest is]]))

(defn start-location [pipes]
  (some identity (for [[i line] (map vector (range) pipes)
                       [j char] (map vector (range) line)]
                   (when (= char \S) [i j]))))

(defn walkable-start-neighbors [pipes [row col]]
  (let [f (fn [next tiles] (when ((set tiles) (get-in pipes next)) next))]
    (filter identity [(f [(dec row) col] "|7F")
                      (f [row (inc col)] "-J7")
                      (f [(inc row) col] "|LJ")
                      (f [row (dec col)] "-LF")])))

(defn walk-pipes [pipes prev curr]
  (let [valid (fn [prev curr next valid-from-tiles valid-to-tiles]
                (when (and (not= next prev)
                           ((set valid-from-tiles) (get-in pipes curr))
                           ((set valid-to-tiles) (get-in pipes next)))
                  next))
        next (fn [prev [row col :as curr]]
               (some identity [(valid prev curr [(dec row) col] "|LJ" "|7F")
                               (valid prev curr [row (inc col)] "-LF" "-J7")
                               (valid prev curr [(inc row) col] "|7F" "|LJ")
                               (valid prev curr [row (dec col)] "-J7" "-LF")]))]
    (map second (iterate (fn [[prev curr]] [curr (next prev curr)]) [prev curr]))))

(defn solve [lines]
  (let [pipes (vec lines)
        start (start-location pipes)
        [next1 next2] (walkable-start-neighbors pipes start)
        path1 (walk-pipes pipes start next1)
        path2 (walk-pipes pipes start next2)]
    (inc (count (take-while false? (map = path1 path2))))))

(deftest start-location-test
  (is (= (start-location ["-L|F7"
                          "7S-7|"
                          "L|7||"
                          "-L-J|"
                          "L|-JF"])
         [1 1])))

(deftest walkable-start-neighbors-test
  (is (= (walkable-start-neighbors ["-L|F7"
                                    "7S-7|"
                                    "L|7||"
                                    "-L-J|"
                                    "L|-JF"]
                                   [1 1])
         [[1 2] [2 1]])))

(deftest walk-pipes-test
  (is (= (take 7 (walk-pipes ["-L|F7"
                              "7S-7|"
                              "L|7||"
                              "-L-J|"
                              "L|-JF"]
                             [1 1]
                             [1 2]))
         [[1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]])))

(deftest solve-test
  (is (= (solve ["-L|F7"
                 "7S-7|"
                 "L|7||"
                 "-L-J|"
                 "L|-JF"])
         4))
  (is (= (solve ["7-F7-"
                 ".FJ|7"
                 "SJLL7"
                 "|F--J"
                 "LJ.LJ"])
         8)))
