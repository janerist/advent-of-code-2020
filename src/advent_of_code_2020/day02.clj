(ns advent-of-code-2020.day02
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ min max letter pwd] (re-find #"(\d+)-(\d+) (\w): (\w+)" line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :letter (nth letter 0)
     :pwd pwd}))

(defn valid? [{:keys [min max letter pwd]}]
  (let [occurrences (count (filter #(= % letter) pwd))]
    (<= min occurrences max)))

(defn valid2? [{:keys [min max letter pwd]}]
  (let [occurrences [(= letter (nth pwd (- min 1)))
                     (= letter (nth pwd (- max 1)))]]
    (= 1 (count (filter #(= true %) occurrences)))))

(defn solve [validfn]
  (->> (slurp "resources/day02.txt")
       (str/split-lines)
       (map parse-line)
       (filter validfn)
       (count)))

(defn solve1 []
  (solve valid?))

(defn solve2 []
  (solve valid2?))
