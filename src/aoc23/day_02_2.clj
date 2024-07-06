(ns aoc23.day-02-2
  (:require [clojure.test :refer [deftest is]]))

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

(defn min-required-color-counts [game]
  (apply merge-with max {:red 0 :green 0 :blue 0} (:combinations game)))

(defn cube-power [colors]
  (* (:red colors) (:green colors) (:blue colors)))

(defn solve [lines]
  (reduce + 0 (map #(-> % parse-game min-required-color-counts cube-power) lines)))

(deftest parse-game-test
  (is (= (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
         {:id 1 :combinations [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]}))
  (is (= (parse-game "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
         {:id 4 :combinations [{:green 1 :red 3 :blue 6} {:green 3 :red 6} {:green 3 :blue 15 :red 14}]})))

(deftest min-required-color-counts-test
  (is (= (min-required-color-counts (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
         {:red 4 :green 2 :blue 6}))
  (is (= (min-required-color-counts (parse-game "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"))
         {:red 14 :green 3 :blue 15})))
