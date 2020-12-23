(ns advent-of-code-2020.core
  (:require [clojure.repl :refer [demunge]])
  (:require [advent-of-code-2020.day01 :as day01])
  (:require [advent-of-code-2020.day02 :as day02])
  (:require [advent-of-code-2020.day03 :as day03])
  (:require [advent-of-code-2020.day04 :as day04])
  (:require [advent-of-code-2020.day05 :as day05])
  (:require [advent-of-code-2020.day06 :as day06])
  (:require [advent-of-code-2020.day07 :as day07])
  (:require [advent-of-code-2020.day08 :as day08])
  (:require [advent-of-code-2020.day09 :as day09])
  (:require [advent-of-code-2020.day10 :as day10])
  (:require [advent-of-code-2020.day11 :as day11])
  (:gen-class))

(def solvers
  [
   [day01/solve1 day01/solve2]
   [day02/solve1 day02/solve2]
   [day03/solve1 day03/solve2]
   [day04/solve1 day04/solve2]
   [day05/solve1 day05/solve2]
   [day06/solve1 day06/solve2]
   [day07/solve1 day07/solve2]
   [day08/solve1 day08/solve2]
   [day09/solve1 day09/solve2]
   [day10/solve1 day10/solve2]
   [day11/solve1 day11/solve2]
   ])

(defn- solver-name [solver]
  (let [dem (demunge (str solver))
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem))]
    (if pretty pretty dem)))

(defn- run-solver [solver]
  (let [solution (time (solver))]
    (println (str (solver-name solver) ": " solution "\n"))))

(defn- solve-day [n]
  (if-let [solver (nth solvers (- n 1) nil)]
    (doseq [s solver] (run-solver s))
    (println "No solvers for day" n)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if-let [day (first args)]
    (solve-day (Integer/parseInt day))
    (println "First argument must be the day to run (1-24)")))
