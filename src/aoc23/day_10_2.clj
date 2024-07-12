(ns aoc23.day-10-2
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
                               (valid prev curr [row (dec col)] "-J7" "-LF")]))
        tail (map second (iterate (fn [[prev curr]] [curr (next prev curr)]) [prev curr]))] ; nil when start is reached
    (cons prev (take-while identity tail))))

(defn resolve-start-tile [start next1 next2]
  (let [sorted-dirs (sort (vec (map #(vec (map - % start)) [next1 next2])))]
    (case sorted-dirs
      [[-1 0] [0 -1]] \J
      [[-1 0] [0 1]] \L
      [[-1 0] [1 0]] \|
      [[0 -1] [0 1]] \-
      [[0 -1] [1 0]] \7
      [[0 1] [1 0]] \F)))

(defn count-enclosed-tiles
  "resolved-pipes must have S replaced with the matching pipe tile"
  [resolved-pipes path]
  (let [path (set path)
        reducer (fn [{state                    :state
                      top->bottom-left->right? :top->bottom-left->right?
                      count                    :count
                      row                      :row
                      col                      :col
                      :as                      acc}
                     tile]
                  (let [clean-tile (if (not (path [row col])) \X tile)
                        diff (case state
                               :out (case clean-tile
                                      \| {:state :in}
                                      \L {:state :out->in :top->bottom-left->right? true}
                                      \F {:state :out->in :top->bottom-left->right? false}
                                      \X {})
                               :out->in (case clean-tile
                                          \- {}
                                          \J {:state (if top->bottom-left->right? :out :in)}
                                          \7 {:state (if top->bottom-left->right? :in :out)})
                               :in (case clean-tile
                                     \| {:state :out}
                                     \L {:state :in->out :top->bottom-left->right? true}
                                     \F {:state :in->out :top->bottom-left->right? false}
                                     \X {:count (inc count)})
                               :in->out (case clean-tile
                                          \- {}
                                          \J {:state (if top->bottom-left->right? :in :out)}
                                          \7 {:state (if top->bottom-left->right? :out :in)}))]
                    (merge acc diff {:col (inc col)})))
        count-enclosed-tiles-in-line (fn [row line]
                                       (:count (reduce reducer {:state :out :count 0 :row row :col 0} line)))]
    (reduce + (map count-enclosed-tiles-in-line (range) resolved-pipes))))

(defn solve [lines]
  (let [pipes (vec lines)
        start (start-location pipes)
        [next1 next2] (walkable-start-neighbors pipes start)
        start-tile (resolve-start-tile start next1 next2)
        resolved-pipes (map #(.replace % \S start-tile) pipes)
        path1 (walk-pipes pipes start next1)]
    (count-enclosed-tiles resolved-pipes path1)))

(deftest resolve-start-tile-test
  (is (= (resolve-start-tile [1 1] [0 1] [1 0]) \J))
  (is (= (resolve-start-tile [1 1] [1 2] [0 1]) \L))
  (is (= (resolve-start-tile [1 1] [0 1] [2 1]) \|))
  (is (= (resolve-start-tile [1 1] [1 2] [1 0]) \-))
  (is (= (resolve-start-tile [1 1] [1 0] [2 1]) \7))
  (is (= (resolve-start-tile [1 1] [2 1] [1 2]) \F))
  )

(deftest count-enclosed-tiles-test
  (is (= (count-enclosed-tiles ["....."
                                ".F-7."
                                ".|.|."
                                ".L-J."
                                "....."]
                               [[1 1] [1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]])
         1)))

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
  (is (= (walk-pipes ["-L|F7"
                      "7S-7|"
                      "L|7||"
                      "-L-J|"
                      "L|-JF"]
                     [1 1]
                     [1 2])
         [[1 1] [1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]])))

(deftest solve-test
  (is (= (solve ["..........."
                 ".S-------7."
                 ".|F-----7|."
                 ".||.....||."
                 ".||.....||."
                 ".|L-7.F-J|."
                 ".|..|.|..|."
                 ".L--J.L--J."
                 "..........."])
         4))
  (is (= (solve [".F----7F7F7F7F-7...."
                 ".|F--7||||||||FJ...."
                 ".||.FJ||||||||L7...."
                 "FJL7L7LJLJ||LJ.L-7.."
                 "L--J.L7...LJS7F-7L7."
                 "....F-J..F7FJ|L7L7L7"
                 "....L7.F7||L7|.L7L7|"
                 ".....|FJLJ|FJ|F7|.LJ"
                 "....FJL-7.||.||||..."
                 "....L---J.LJ.LJLJ..."])
         8))
  (is (= (solve ["FF7FSF7F7F7F7F7F---7"
                 "L|LJ||||||||||||F--J"
                 "FL-7LJLJ||||||LJL-77"
                 "F--JF--7||LJLJ7F7FJ-"
                 "L---JF-JLJ.||-FJLJJ7"
                 "|F|F-JF---7F7-L7L|7|"
                 "|FFJF7L7F-JF7|JL---7"
                 "7-L-JL7||F7|L7F-7F7|"
                 "L.L7LFJ|||||FJL7||LJ"
                 "L7JLJL-JLJLJL--JLJ.L"])
         10)))
