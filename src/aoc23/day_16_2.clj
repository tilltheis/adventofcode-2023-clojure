(ns aoc23.day-16-2
  (:require [clojure.test :refer [deftest is]]))

(defn redirect-beam [[row-pos col-pos] [row-dir col-dir]]
  [[(+ row-pos row-dir) (+ col-pos col-dir)]
   [row-dir col-dir]])
(defn up-beam [pos] (redirect-beam pos [-1 0]))
(defn down-beam [pos] (redirect-beam pos [1 0]))
(defn left-beam [pos] (redirect-beam pos [0 -1]))
(defn right-beam [pos] (redirect-beam pos [0 1]))

(defn count-energized-tiles [grid entry-beam]
  (let [grid (vec grid)
        visited (loop [visited #{}
                       [[[row-pos col-pos :as pos]
                         [row-dir col-dir :as dir]
                         :as beam]
                        & beams] (list entry-beam)]
                  (cond
                    (nil? beam) visited
                    (or (not (< -1 row-pos (count grid)))
                        (not (< -1 col-pos (count (first grid))))
                        (visited beam)) (recur visited beams)
                    :else (let [new-beams (case (get-in grid pos)
                                            \. (cons (redirect-beam pos dir) beams)
                                            \/ (case dir
                                                 [1 0] (cons (left-beam pos) beams)
                                                 [-1 0] (cons (right-beam pos) beams)
                                                 [0 1] (cons (up-beam pos) beams)
                                                 [0 -1] (cons (down-beam pos) beams))
                                            \\ (case dir
                                                 [0 1] (cons (down-beam pos) beams)
                                                 [0 -1] (cons (up-beam pos) beams)
                                                 [1 0] (cons (right-beam pos) beams)
                                                 [-1 0] (cons (left-beam pos) beams))
                                            \| (case col-dir
                                                 0 (cons (redirect-beam pos dir) beams)
                                                 (cons (up-beam pos) (cons (down-beam pos) beams)))
                                            \- (case row-dir
                                                 0 (cons (redirect-beam pos dir) beams)
                                                 (cons (right-beam pos) (cons (left-beam pos) beams))))]
                            (recur (conj visited beam) new-beams))))]
    (count (set (map first visited)))))

(defn solve [lines]
  (let [grid (vec lines)
        vertical-entry-beams (for [[row y-dir] [[0 1] [(dec (count grid)) -1]]
                                   col (range (count (first grid)))]
                               [[row col] [y-dir 0]])
        horizontal-entry-beams (for [row (range (count grid))
                                     [col x-dir] [[0 1] [(dec (count (first grid))) -1]]]
                                 [[row col] [0 x-dir]])
        entry-beams (concat vertical-entry-beams horizontal-entry-beams)]
    (apply max (map #(count-energized-tiles grid %) entry-beams))))

(deftest solve-test
  (is (= (solve [".|...\\...."
                 "|.-.\\....."
                 ".....|-..."
                 "........|."
                 ".........."
                 ".........\\"
                 "..../.\\\\.."
                 ".-.-/..|.."
                 ".|....-|.\\"
                 "..//.|...."]) 51)))
