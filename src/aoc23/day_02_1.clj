(ns aoc23.day-02-1
  (:require [clojure.test :refer [deftest is]]))

(def max-red-count 12)
(def max-green-count 13)
(def max-blue-count 14)

(defn parse-game [str]
  (let [[game-str rest-str] (.split str ": ")
        id (Integer/parseInt (.substring game-str 5))
        combination-strs (.split rest-str "; ")
        parse-color (fn [cube-str]
                      (let [[count-str color-str] (.split cube-str " ")]
                        {(keyword color-str) (Integer/parseInt count-str)}))
        parse-combination (fn [combination-str]
                            (let [cube-strs (.split combination-str ", ")
                                  colors (map parse-color cube-strs)]
                              (reduce conj {} colors)))
        combinations (map parse-combination combination-strs)]
    {:id id :combinations combinations}))

(defn- valid-color? [color limit m]
  (let [count (color m)]
    (or (nil? count) (<= count limit))))

(defn possible-game? [game]
  (every?
    #(and
       (valid-color? :red max-red-count %)
       (valid-color? :green max-green-count %)
       (valid-color? :blue max-blue-count %))
    (seq (:combinations game))))

(defn solve [lines]
  (reduce + 0 (map :id (filter possible-game? (map parse-game lines)))))

(deftest parse-game-test
  (is (= (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
         {:id 1 :combinations [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]}))
  (is (= (parse-game "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
         {:id 4 :combinations [{:green 1 :red 3 :blue 6} {:green 3 :red 6} {:green 3 :blue 15 :red 14}]})))

(deftest valid-color?-test
  (is (true? (valid-color? :red 3 {:red 3})))
  (is (false? (valid-color? :red 3 {:red 4})))
  (is (true? (valid-color? :red 3 {:green 4}))))

(deftest possible-game?-test
  (is (true? (possible-game? {:id 1 :combinations [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]})))
  (is (false? (possible-game? {:id 4 :combinations [{:green 1 :red 3 :blue 6} {:green 3 :red 6} {:green 3 :blue 15 :red 14}]}))))
