(ns aoc23.solver
  (:require [clojure.java.io :as io])
  (:import (java.io FileNotFoundException)))

(def solver-ns-name (ns-name *ns*))

(defn eprintln [& args] (binding [*out* *err*] (apply println args)))

(defn usage []
  (eprintln "Usage: clj -M -m" solver-ns-name "<day> <part> <puzzle-input-path>")
  (eprintln "Example: clj -M -m" solver-ns-name "1 2 resources/input_01.txt")
  (System/exit 1))

(defn -main [& args]
  (if (not= (count args) 3)
    (usage)
    (let [[day part input-path] args
          padded-day (if (< (.length day) 2) (str "0" day) day)
          ns-name (symbol (str "aoc23.day-" padded-day "-" part))
          _ (try (require ns-name)
                 (catch FileNotFoundException _
                   (eprintln "Solution to day" day "part" part "not found.")
                   (System/exit 2)))
          ns (find-ns ns-name)
          solve (ns-resolve ns 'solve)
          solution (try (with-open [reader (io/reader input-path)] (solve (line-seq reader)))
                        (catch FileNotFoundException _
                          (eprintln "Puzzle input file" input-path "not found.")
                          (System/exit 3)))]
      (println solution))))