(ns advent-of-code-2020.day07
  (:require [clojure.string :as str]
            [clojure.set :refer [union]]))

; light red bags => :light-red
(defn bag-color [bag]
  (let [[_ adj color] (re-matches #"(\w+) (\w+) bag[s\.]*" bag)]
    (keyword (str adj "-" color))))

;2 muted yellow bags => [:muted-yellow 2]
(defn inner-bag [inner-bag]
  (let [[count bag] (str/split inner-bag #" " 2)]
    [(bag-color bag) (Integer/parseInt count)]))

(defn parse-line [line]
  (let [[bag contain] (str/split line #" contain ")
        bag-color (bag-color bag)
        inner-bags (filter #(not (= "no other bags." %)) (str/split contain #", "))]
    [bag-color (into {} (map inner-bag inner-bags))]))

(defn parse-bags [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-line)))

(defn can-contain [bags bag-key]
  (filter #(bag-key (last %)) bags))

(defn bags-containing [bags bag-key acc]
  (let [found-bags (map first (can-contain bags bag-key))
        count (count found-bags)]
    (if (= 0 count)
      acc
      (reduce
        #(bags-containing bags %2 %1)
        (union acc (set found-bags))
        found-bags))))

(defn solve1 []
  (->> (bags-containing (parse-bags "resources/day07.txt") :shiny-gold #{})
       (count)))

(defn containing-bags [bags bag-key acc multiplier]
  (let [[_ contain] (first (filter #(= (first %) bag-key) bags))]
    (if (empty? contain)
      acc
      (reduce
        (fn [a [k v]] (containing-bags bags k a (* multiplier v)))
        (concat acc (map (fn [[k v]] (repeat (* multiplier v) k)) contain))
        contain))))

(defn solve2 []
  (->>
    (containing-bags (parse-bags "resources/day07.txt") :shiny-gold [] 1)
    (flatten)
    (count)))