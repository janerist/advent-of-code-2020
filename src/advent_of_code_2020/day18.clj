(ns advent-of-code-2020.day18
  (:require [clojure.string :as str]))

(defn tokenize [expression]
  (->> (str/split expression #"")
       (filter #(not= " " %))))

(defn has-greater-or-equal-precedence [precedence op1 op2]
  (>= (get precedence op1 -1) (get precedence op2 -1)))

; Shunting yard algorithm
(defn to-rpn [tokens precedence]
  (->> tokens
       (reduce (fn [[output stack] token]
                 (cond
                   (or (= "+" token) (= "*" token))
                   [(vec (concat output (take-while #(has-greater-or-equal-precedence precedence % token) stack)))
                    (cons token (drop-while #(has-greater-or-equal-precedence precedence % token) stack))]

                   (= "(" token)
                   [output (cons token stack)]

                   (= ")" token)
                   [(vec (concat output (take-while #(not= "(" %) stack)))
                    (rest (drop-while #(not= "(" %) stack))]

                   :else
                   [(conj output (Integer/parseInt token)) stack]))
               [[] ()])
       (apply concat)))

(defn evaluate [rpn]
  (let [ops {"+" +, "*" *}]
    (->> rpn
         (reduce (fn [stack token]
                   (if (contains? ops token)
                     (cons ((ops token) (second stack) (first stack)) (drop 2 stack))
                     (cons token stack)))
                 [])
         (first))))

(defn solve [precedence]
  (->> (slurp "resources/day18.txt")
       (str/split-lines)
       (map tokenize)
       (map #(to-rpn % precedence))
       (map evaluate)
       (apply +)))

(defn solve1 []
  (solve {"+" 0 "*" 0}))

(defn solve2 []
  (solve {"+" 1 "*" 0}))
