(ns aoc23.day-08-1
  (:require [clojure.test :refer [deftest is]]))

(defn parse-map [[directions-str _ & network-lines]]
  (let [directions (map #(case % \R :right \L :left) directions-str)
        nodes (map (fn [l] (let [[name left right] (re-seq #"\w+" l)] {name {:left left :right right}})) network-lines)]
    {:directions directions :network (apply merge nodes)}))

(defn solve [lines]
  (let [{directions :directions network :network} (parse-map lines)
        reducer (fn [{count :count node :node} dir]
                  (let [new-node (dir (network node))
                        f (if (= new-node "ZZZ") reduced identity)]
                    (f {:count (inc count) :node new-node})))
        {count :count} (reduce reducer {:count 0 :node "AAA"} (cycle directions))]
    count))

(deftest parse-map-test
  (is (= (parse-map ["LLR"
                     ""
                     "AAA = (BBB, BBB)"
                     "BBB = (AAA, ZZZ)"
                     "ZZZ = (ZZZ, ZZZ)"])
         {:directions [:left :left :right]
          :network    {"AAA" {:left "BBB" :right "BBB"}
                       "BBB" {:left "AAA" :right "ZZZ"}
                       "ZZZ" {:left "ZZZ" :right "ZZZ"}}})))

(deftest solve-test
  (is (= (solve ["RL"
                 ""
                 "AAA = (BBB, CCC)"
                 "BBB = (DDD, EEE)"
                 "CCC = (ZZZ, GGG)"
                 "DDD = (DDD, DDD)"
                 "EEE = (EEE, EEE)"
                 "GGG = (GGG, GGG)"
                 "ZZZ = (ZZZ, ZZZ)"])
         2))
  (is (= (solve ["LLR"
                 ""
                 "AAA = (BBB, BBB)"
                 "BBB = (AAA, ZZZ)"
                 "ZZZ = (ZZZ, ZZZ)"])
         6)))
