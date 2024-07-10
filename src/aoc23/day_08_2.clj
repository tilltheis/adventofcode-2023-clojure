(ns aoc23.day-08-2
  (:require [clojure.test :refer [deftest is]]))

(defn parse-map [[directions-str _ & network-lines]]
  (let [directions (map #(case % \R :right \L :left) directions-str)
        nodes (map (fn [l] (let [[name left right] (re-seq #"\w+" l)] {name {:left left :right right}})) network-lines)]
    {:directions directions :network (apply merge nodes)}))

(defn least-common-multiple [x & xs]
  (when (or (zero? x) (some zero? xs)) (throw (IllegalArgumentException. "zero not allowed")))
  (let [all-xs (cons x xs)
        max-x (apply max all-xs)
        lcm? (fn [candidate] (every? #(= 0 (mod candidate %)) all-xs))]
    (some #(when (lcm? %) %) (iterate #(+ % max-x) max-x))))

(defn solve [lines]
  (let [{directions :directions network :network} (parse-map lines)
        start-nodes (filter #(.endsWith % "A") (keys network))
        reducer (fn [{count :count node :node} dir]
                  (let [new-node (dir (network node))
                        ; stop after first finish because finish nodes repeat at constant interval
                        f (if (.endsWith new-node "Z") reduced identity)]
                    (f {:count (inc count) :node new-node})))
        steps-from-starts (map (fn [n] (:count (reduce reducer {:count 0 :node n} (cycle directions)))) start-nodes)]
    (apply least-common-multiple steps-from-starts)))

(deftest least-common-multiple-test
  (is (thrown? IllegalArgumentException (least-common-multiple 0)))
  (is (= (least-common-multiple 1) 1))
  (is (= (least-common-multiple 1 5) 5))
  (is (= (least-common-multiple 2 3 5) 30))
  (is (= (least-common-multiple 4 7 9 13) 3276)))

(deftest parse-map-test
  (is (= (parse-map ["LLR"
                     ""
                     "11A = (11B, XXX)"
                     "11B = (XXX, 11Z)"])
         {:directions [:left :left :right]
          :network    {"11A" {:left "11B" :right "XXX"}
                       "11B" {:left "XXX" :right "11Z"}}})))

(deftest solve-test
  (is (= (solve ["LR"
                 ""
                 "11A = (11B, XXX)"
                 "11B = (XXX, 11Z)"
                 "11Z = (11B, XXX)"
                 "22A = (22B, XXX)"
                 "22B = (22C, 22C)"
                 "22C = (22Z, 22Z)"
                 "22Z = (22B, 22B)"
                 "XXX = (XXX, XXX)"])
         6)))