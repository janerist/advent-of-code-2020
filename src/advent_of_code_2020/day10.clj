(ns advent-of-code-2020.day10
  (:require [clojure.string :as str]))

; Gets a list of the adapters output joltage
; Each adapter can take an input 1, 2, or 3 jolts lower than its output and still work
(defn parse-adapters [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map #(Integer/parseInt %))
       (set)))

(defn works? [adapter input]
  (<= (- adapter 3) input adapter))

(defn builtin-adapter [adapters]
  (+ 3 (apply max adapters)))

(defn select-adapter [adapters input]
  (->> (filter #(works? % input) adapters)
       (apply min)))

(defn get-diffs [outlet adapters device]
  (loop [remaining (conj adapters device)
         adapter outlet
         diffs []]
    (if (empty? remaining)
      diffs
      (let [next (select-adapter remaining adapter)
            diff (- next adapter)]
        (recur (disj remaining next) next (conj diffs diff))))))

(defn solve1 []
  (let [outlet 0
        adapters (parse-adapters "resources/day10.txt")
        device (builtin-adapter adapters)
        diffs (get-diffs outlet adapters device)
        {ones 1 threes 3} (frequencies diffs)]
    (* ones threes)))

(def count-arrangements
  (memoize
    (fn [x y & rest]
      (cond
        (nil? rest) 1
        (> (+ x y) 3) (apply count-arrangements y rest)
        :else (+ (apply count-arrangements y rest) (apply count-arrangements (+ x y) rest))))))

(defn solve2 []
  (let [outlet 0
        adapters (parse-adapters "resources/day10.txt")
        device (builtin-adapter adapters)
        diffs (get-diffs outlet adapters device)]
    (apply count-arrangements diffs)))
