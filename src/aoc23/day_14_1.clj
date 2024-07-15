(ns aoc23.day-14-1
  (:require [clojure.test :refer [deftest is]]))

(defn tilt-north [platform]
  (let [reducer (fn [{col :col stack :stack moving-rock-count :moving-rock-count :as state} row]
                  (let [tile (nth (nth platform row nil) col nil)
                        diff (case tile
                               \. {}
                               \O {:moving-rock-count (inc moving-rock-count)}
                               \# {:stack             (vec (concat stack
                                                                   (repeat moving-rock-count \O)
                                                                   (repeat (- row (count stack) moving-rock-count) \.)
                                                                   [\#]))
                                   :moving-rock-count 0}
                               nil {:stack             (vec (concat stack
                                                                    (repeat moving-rock-count \O)
                                                                    (repeat (- row (count stack) moving-rock-count) \.)))
                                    :moving-rock-count 0})]
                    (merge state diff)))
        tilt-column (fn [col] (:stack (reduce reducer
                                              {:col col :stack [] :moving-rock-count 0}
                                              (range 0 (inc (count platform))))))
        tilted-columns (mapv tilt-column (range (count (first platform))))]
    (vec (for [row (range (count platform))]
           (apply str (for [col (range (count (first platform)))]
                        (get-in tilted-columns [col row])))))))

(defn weigh [platform]
  (let [height (count platform)
        width (count (first platform))
        weights (for [row (range height)
                      col (range width)]
                  (if (= \O (get-in platform [row col])) (- height row) 0))]
    (reduce + weights)))

(defn solve [lines]
  (weigh (tilt-north (vec lines))))

(deftest tilt-north-test
  (is (= (tilt-north ["O....#...."
                      "O.OO#....#"
                      ".....##..."
                      "OO.#O....O"
                      ".O.....O#."
                      "O.#..O.#.#"
                      "..O..#O..O"
                      ".......O.."
                      "#....###.."
                      "#OO..#...."])
         ["OOOO.#.O.."
          "OO..#....#"
          "OO..O##..O"
          "O..#.OO..."
          "........#."
          "..#....#.#"
          "..O..#.O.O"
          "..O......."
          "#....###.."
          "#....#...."])))

(deftest solve-test
  (is (= (solve ["O....#...."
                 "O.OO#....#"
                 ".....##..."
                 "OO.#O....O"
                 ".O.....O#."
                 "O.#..O.#.#"
                 "..O..#O..O"
                 ".......O.."
                 "#....###.."
                 "#OO..#...."])
         136)))
