(ns aoc23.day-14-2
  (:require [clojure.test :refer [deftest is]]))

(defn tilt-north-and-rotate-clockwise [platform]
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
    (vec (for [col (range (count (first platform)))]
           (apply str (for [row (range (dec (count platform)) -1 -1)]
                        (get-in tilted-columns [col row])))))))

(defn tilt-cycle [platform]
  (let [north-tilted (tilt-north-and-rotate-clockwise platform)
        west-tilted (tilt-north-and-rotate-clockwise north-tilted)
        south-tilted (tilt-north-and-rotate-clockwise west-tilted)
        east-tilted (tilt-north-and-rotate-clockwise south-tilted)]
    east-tilted))

(defn weigh [platform]
  (let [height (count platform)
        width (count (first platform))
        weights (for [row (range height)
                      col (range width)]
                  (if (= \O (get-in platform [row col])) (- height row) 0))]
    (reduce + weights)))

(defn solve [lines]
  (let [platforms (iterate tilt-cycle (vec lines))
        {cache             :cache
         prefix-length     :prefix-length
         repetition-length :repetition-length} (reduce
                                                 (fn [cache x]
                                                   (if-let [index (first (keep-indexed #(when (= %2 x) %1) cache))]
                                                     (reduced {:cache             cache
                                                               :prefix-length     index
                                                               :repetition-length (- (count cache) index)})
                                                     (conj cache x)))
                                                 []
                                                 platforms)
        n 1000000000
        repetition-index (mod (- n prefix-length) repetition-length)
        cache-index (+ prefix-length repetition-index)]
    (weigh (nth cache cache-index))))

(deftest tilt-cycle-test
  (let [platform ["O....#...."
                  "O.OO#....#"
                  ".....##..."
                  "OO.#O....O"
                  ".O.....O#."
                  "O.#..O.#.#"
                  "..O..#O..O"
                  ".......O.."
                  "#....###.."
                  "#OO..#...."]]
    (is (= (tilt-cycle platform) [".....#...."
                                  "....#...O#"
                                  "...OO##..."
                                  ".OO#......"
                                  ".....OOO#."
                                  ".O#...O#.#"
                                  "....O#...."
                                  "......OOOO"
                                  "#...O###.."
                                  "#..OO#...."]))
    (is (= (tilt-cycle (tilt-cycle platform)) [".....#...."
                                               "....#...O#"
                                               ".....##..."
                                               "..O#......"
                                               ".....OOO#."
                                               ".O#...O#.#"
                                               "....O#...O"
                                               ".......OOO"
                                               "#..OO###.."
                                               "#.OOO#...O"]))))

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
         64)))
