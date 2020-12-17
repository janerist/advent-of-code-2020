(ns advent-of-code-2020.day03
  (:require [clojure.string :as str]))

; - Map repeats indefinitely to the right
; - Start in top left square
; - Have to reach below the bottom row

(defn parse-map [filename]
  (->> (slurp filename)
       (str/split-lines)))

(defn get-location [map x y]
  (let [row (nth map y)]
    (nth row (mod x (count row)))))

(defn trees-in-slope [map right down]
  (loop [trees 0
         x right
         y down]
    (cond
      (>= y (count map)) trees
      (= \# (get-location map x y)) (recur (inc trees) (+ x right) (+ y down))
      :else (recur trees (+ x right) (+ y down)))))

(defn solve1 []
  (let [map (parse-map "resources/day03.txt")]
    (trees-in-slope map 3 1)))

(defn solve2 []
  (let [map (parse-map "resources/day03.txt")]
    (*
      (trees-in-slope map 1 1)
      (trees-in-slope map 3 1)
      (trees-in-slope map 5 1)
      (trees-in-slope map 7 1)
      (trees-in-slope map 1 2))))
