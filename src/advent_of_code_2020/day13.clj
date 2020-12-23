(ns advent-of-code-2020.day13
  (:require [clojure.string :as str]))

; Bus ID indicates how often the bus leaves for the aiport
; Bus ID 5 leaves at timestamps 0, 5, 10, 15, 20 etc

(defn parse-input [input]
  (let [[line1 line2] (str/split-lines input)
        timestamp (Integer/parseInt line1)
        buses (->> line2
                   (re-seq #"\d+")
                   (map #(Integer/parseInt %)))]
    [timestamp buses]))

(defn minutes-away [timestamp bus]
  (let [r (mod timestamp bus)]
    (if (= 0 r)
      0
      (- bus r))))

(defn solve1 []
  (let [input (slurp "resources/day13.txt")
        [timestamp buses] (parse-input input)]
    (->> buses
         (map (fn [id] [id (minutes-away timestamp id)]))
         (sort-by second)
         (first)
         (apply *))))

(defn parse-input2 [input]
  (let [[_ line2] (str/split-lines input)]
    (->> (str/split line2 #",")
         (map-indexed (fn [idx id] [id idx]))
         (filter #(not= "x" (first %)))
         (map (fn [[id offset]] [(Integer/parseInt id) offset])))))

(defn reduce-it [[product sum] [id idx]]
  (loop [s sum]
    (if (= 0 (mod (+ s idx) id))
      [(* product id) s]
      (recur (+ s product)))))

(defn solve2 []
  (let [input (slurp "resources/day13.txt")
        buses (parse-input2 input)]
    (->> buses
         (reduce reduce-it)
         (second))))
