(ns advent-of-code-2020.day14
  (:require [clojure.string :as str]))

(def test-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0")

; mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
(defn parse-mask [line]
  (let [[_ mask] (re-find #"mask = ([01X]+)" line)]
    mask))

; mem[8] = 11
(defn parse-line [line]
  (let [[_ address value] (re-find #"mem\[(\d+)\] = (\d+)" line)]
    [(Integer/parseInt address) (Integer/parseInt value)]))

(defn apply-mask [mask value]
  (let [bin (Integer/toBinaryString value)
        padded (str (.repeat "0" (- 36 (count bin))) bin)]
    (loop [value mask
           idx 0]
      (if (>= idx (count value))
        (Long/parseLong value 2)
        (recur
          (if (= \X (nth value idx))
            (str (subs value 0 idx) (nth padded idx) (subs value (inc idx)))
            value)
          (inc idx))))))

(defn run-program [state line]
  (if (.startsWith line "mask")
    (assoc state :mask (parse-mask line))
    (let [[address value] (parse-line line)
          masked (apply-mask (:mask state) value)]
      (assoc-in state [:memory address] masked))))

(defn solve1 []
  (let [input (slurp "resources/day14.txt")
        program (str/split-lines input)
        state {:memory {} :mask nil}]
    (->> (reduce run-program state program)
         (:memory)
         (vals)
         (apply +))))
