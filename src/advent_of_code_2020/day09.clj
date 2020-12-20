(ns advent-of-code-2020.day09
  (:require [clojure.string :as str]))

; XMAS series of numbers
; First 25 numbers is preamble
; After preamble each number is the sum of any two of the 25 previous numbers
; The two numbers will have different values and there might be more than one pair

(defn parse-numbers [filename]
  (->> (slurp filename)
       (str/split-lines)
       (mapv #(Long/parseLong %))))

; Generates a lazy seq of pairs in numbers that sum to sum
(defn sum-pairs [numbers sum]
  (for [x numbers
        y numbers
        :when (not (= x y))
        :when (= sum (+ x y))]
    [x y]))

(defn find-invalid-number [numbers preamble]
  (->> (subvec numbers preamble)
       (map-indexed (fn [idx sum]
                      (sum-pairs (subvec numbers idx (+ idx preamble)) sum)))
       (take-while #(not (empty? %)))
       (count)
       (+ preamble)
       (nth numbers)))

(defn solve1 []
  (let [numbers (parse-numbers "resources/day09.txt")
        preamble 25]
    (find-invalid-number numbers preamble)))

(defn solve2 []
  (let [numbers (parse-numbers "resources/day09.txt")
        preamble 25
        invalid (find-invalid-number numbers preamble)]
    (loop [idx 0
           range []]
      (let [sum (reduce + range)]
        (cond
          ; Found the correct sum
          (= invalid sum)
          (let [sorted (sort range)]
            (+ (first sorted) (last sorted)))

          ; Sum was too big, move to the next index and start over
          (> sum invalid)
          (recur (inc idx) [])

          ; Add the next number to the current range
          :else
          (let [next-index (+ idx 1 (count range))
                next (nth numbers next-index)]
            (recur idx (conj range next))))))))
