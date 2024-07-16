; same as day 16 part 2 but using java.util HashSet and arrays instead of native clojure types
(ns aoc23.day-16-3
  (:require [clojure.test :refer [deftest is]])
  (:import (java.util HashSet)
           (java.util.function Function)
           (java.util.stream Collectors)))

(defn redirect-beam [[row-pos col-pos] [row-dir col-dir]]
  [[(+ row-pos row-dir) (+ col-pos col-dir)]
   [row-dir col-dir]])
(defn up-beam [pos] (redirect-beam pos [-1 0]))
(defn down-beam [pos] (redirect-beam pos [1 0]))
(defn left-beam [pos] (redirect-beam pos [0 -1]))
(defn right-beam [pos] (redirect-beam pos [0 1]))

(defn count-energized-tiles [^"[[C" grid entry-beam]
  (let [^HashSet visited (loop [visited (HashSet.)
                                [[[row-pos col-pos :as pos]
                                  [row-dir col-dir :as dir]
                                  :as beam]
                                 & beams] (list entry-beam)]
                           (cond
                             (nil? beam) visited
                             (or (not (< -1 row-pos (alength grid)))
                                 (not (< -1 col-pos (alength ^chars (aget grid 0))))
                                 (.contains visited beam)) (recur visited beams)
                             :else (let [new-beams (case (aget ^chars (aget grid row-pos) col-pos)
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
                                                     \| (case (int col-dir)
                                                          0 (cons (redirect-beam pos dir) beams)
                                                          (cons (up-beam pos) (cons (down-beam pos) beams)))
                                                     \- (case (int row-dir)
                                                          0 (cons (redirect-beam pos dir) beams)
                                                          (cons (right-beam pos) (cons (left-beam pos) beams))))]
                                     (.add visited beam)
                                     (recur visited new-beams))))]
    (-> (.stream visited)
        (.map (reify Function (apply [_ arg] (first arg))))
        (.collect (Collectors/toSet))
        (#(.size ^HashSet %)))))

(defn solve [lines]
  (let [grid (into-array (map char-array lines))
        vertical-entry-beams (for [[row row-dir] [[0 1] [(dec (count grid)) -1]]
                                   col (range (count (first grid)))]
                               [[row col] [row-dir 0]])
        horizontal-entry-beams (for [row (range (count grid))
                                     [col col-dir] [[0 1] [(dec (count (first grid))) -1]]]
                                 [[row col] [0 col-dir]])
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
