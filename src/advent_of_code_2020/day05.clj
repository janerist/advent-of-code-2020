(ns advent-of-code-2020.day05
  (:require [clojure.string :as str]))

(defn seat-id [seat]
  (-> seat
      (str/replace #"F|L" "0")
      (str/replace #"B|R" "1")
      (Integer/parseInt 2)))

(defn solve1 []
  (->> (slurp "resources/day05.txt")
       (str/split-lines)
       (map seat-id)
       (apply max)))

(defn solve2 []
  (->> (slurp "resources/day05.txt")
       (str/split-lines)
       (map seat-id)
       (sort)
       (reduce (fn [x y]
                 (let [n (inc x)]
                   (if (= n (dec y))
                     (reduced n)
                     y))))))