(ns aoc23.solver
  (:require [clojure.java.io :as io])
  (:import (java.io FileNotFoundException)))

(defn eprintln [& args] (binding [*out* *err*] (apply println args)))

(defn usage [tool-alias]
  (eprintln (str "Usage: clj -M:" tool-alias " <day> <part> <puzzle-input-path>"))
  (eprintln (str "Example: clj -M:" tool-alias " 1 2 resources/input_01.txt"))
  (System/exit 1))

(defn -main* [tool-alias & args]
  (if (not= (count args) 3)
    (usage tool-alias)
    (let [[day part input-path] args
          padded-day (if (< (.length day) 2) (str "0" day) day)
          ns-name (symbol (str "aoc23.day-" padded-day "-" part))
          _ (try (require ns-name)
                 (catch FileNotFoundException _
                   (eprintln "Solution to day" day "part" part "not found.")
                   (System/exit 2)))
          ns (find-ns ns-name)
          solve (ns-resolve ns 'solve)
          [solution millis] (try (with-open [reader (io/reader input-path)]
                                   (let [before (System/currentTimeMillis)
                                         solution (solve (line-seq reader))
                                         after (System/currentTimeMillis)]
                                     [solution (- after before)]))
                                 (catch FileNotFoundException _
                                   (eprintln "Puzzle input file" input-path "not found.")
                                   (System/exit 3)))]
      (println (str solution " (" millis "ms)")))))

(defn -main [& args] (apply -main* "solve" args))
