(ns advent-of-code-2020.day01
  (:require [clojure.string :as str]))

(def expense-report
  (->> (slurp "resources/day01.txt")
       (str/split-lines)
       (map #(Integer/parseInt %))))

(defn solve1 []
  (->> (for [x1 expense-report
             x2 expense-report
             :when (< x1 x2)
             :when (= 2020 (+ x1 x2))]
         [x1 x2])
       (first)
       (reduce *)))

(defn solve2 []
  (->> (for [x1 expense-report
             x2 expense-report
             x3 expense-report
             :when (< x1 x2)
             :when (< x2 x3)
             :when (= 2020 (+ x1 x2 x3))]
         [x1 x2 x3])
       (first)
       (reduce *)))

