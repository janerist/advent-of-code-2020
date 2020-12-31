(ns advent-of-code-2020.day19
  (:require [clojure.string :as str]))

(defn parse-rule [line]
  (let [[_ idx rule] (re-matches #"(\d+): (.*)" line)]
    [(Integer/parseInt idx) (str/split (str/escape rule {\" ""}) #" ")]))

(defn subst [rules rule]
  (loop [[token & rest] rule
         acc []]
    (if (nil? token)
      (flatten (if (= (count acc) 1)
                 acc
                 (cons "(" (conj acc ")"))))
      (if (or (= "a" token) (= "b" token) (= "|" token) (= ")" token) (= "(" token))
        (recur rest (conj acc token))
        (recur rest (conj acc (subst rules (nth rules (Integer/parseInt token)))))))))

(defn solve1 []
  (let [input (slurp "resources/day19.txt")
        [top bottom] (str/split input #"\n\n")
        rules (map second (sort-by first (mapv parse-rule (str/split-lines top))))
        messages (str/split-lines bottom)
        pattern (->> (first rules)
                     (subst rules)
                     (apply str)
                     (re-pattern))]
    (count (filter #(re-matches pattern %) messages))))

(defn solve2 []
  (let [input (slurp "resources/day19.txt")
        [top bottom] (str/split input #"\n\n")
        rules (-> (->> (str/split-lines top)
                       (map parse-rule)
                       (sort-by first)
                       (mapv second))
                  ; Works for my input, can't be bothered to generalise it...
                  (assoc 8 ["42"
                            "|" "42" "42"
                            "|" "42" "42" "42"
                            "|" "42" "42" "42" "42"
                            "|" "42" "42" "42" "42" "42"])
                  (assoc 11 ["42" "31"
                             "|" "42" "42" "31" "31"
                             "|" "42" "42" "42" "31" "31" "31"
                             "|" "42" "42" "42" "42" "31" "31" "31" "31"]))
        messages (str/split-lines bottom)
        pattern (->> (first rules)
                     (subst rules)
                     (apply str)
                     (re-pattern))]
    (count (filter #(re-matches pattern %) messages))))
