(ns advent-of-code-2020.day14
  (:require [clojure.string :as str]))

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

;--------------------------------------------------------------------------------

(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian-product (rest colls))
          x (first colls)]
      (vec (cons x more)))))

(defn floating-combos [mask]
  (let [maskvec (vec mask)
        idxes (keep-indexed #(if (= \X %2) %1) maskvec)
        colls (map #(vector [% 0] [% 1]) idxes)
        cp (cartesian-product colls)]
    (map #(->> %
               (reduce (fn [res [idx val]] (assoc res idx val)) maskvec)
               (apply str))
         cp)))

;000000000000000000000000000000101010 (address 42)
;000000000000000000000000000000X1001X (mask)
;000000000000000000000000000000X1101X (result)

(defn apply-mask2 [mask value]
  (let [bin (Integer/toBinaryString value)
        padded (str (.repeat "0" (- 36 (count bin))) bin)]
    (loop [value (vec padded)
           idx 0]
      (if (>= idx (count value))
        (apply str value)
        (recur
          (case (nth mask idx)
            \0 value
            \1 (assoc value idx \1)
            \X (assoc value idx \X))
          (inc idx))))))

(defn run-program2 [state line]
  (if (.startsWith line "mask")
    (assoc state :mask (parse-mask line))
    (let [[address value] (parse-line line)
          masked (apply-mask2 (:mask state) address)
          combos (floating-combos masked)]
      (reduce #(assoc-in %1 [:memory (Long/parseLong %2 2)] value) state combos))))

(defn solve2 []
  (let [input (slurp "resources/day14.txt")
        program (str/split-lines input)
        state {:memory {} :mask nil}]
    (->> (reduce run-program2 state program)
         (:memory)
         (vals)
         (apply +))))
