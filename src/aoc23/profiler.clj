(ns aoc23.profiler
  (:require [aoc23.solver :as solver]
            [clj-async-profiler.core :as prof]))

(defn -main [& args]
  (prof/profile (apply solver/-main* "profile" args))
  (println "Profiling data has been written to /tmp/clj-async-profiler/results/"))