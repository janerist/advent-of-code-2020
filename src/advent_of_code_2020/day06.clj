(ns advent-of-code-2020.day06
  (:require [clojure.set :refer [union intersection]]
            [clojure.string :as str]))

(defn group [setfn g]
  (->> (str/split g #"\n")
       (map set)
       (apply setfn)))

(def group-union
  (partial group union))

(def group-intersection
  (partial group intersection))

(defn solve [setfn]
  (->> (str/split (slurp "resources/day06.txt") #"\n\n")
       (map setfn)
       (map count)
       (reduce +)))

(defn solve1 []
  (solve group-union))

(defn solve2 []
  (solve group-intersection))
