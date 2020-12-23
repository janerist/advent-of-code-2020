(ns advent-of-code-2020.day11
  (:require [clojure.string :as str]))

(def directions
  [[dec identity]                                           ; left
   [dec dec]                                                ; top left
   [identity dec]                                           ; top
   [inc dec]                                                ; top right
   [inc identity]                                           ; right
   [inc inc]                                                ; bottom right
   [identity inc]                                           ; bottom
   [dec inc]])                                              ; bottom left

(defn parse-layout [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map seq)))

(defn find-seat [layout x y]
  (nth (nth layout y) x))

(defn valid? [w h x y]
  (and (< -1 x w) (< -1 y h)))

(defn first-visible-seat [area]
  (first (filter #(or (= \L %) (= \# %)) area)))

(defn dir-iterator [[xfn yfn] pos]
  (drop 1 (iterate (fn [[x y]] [(xfn x) (yfn y)]) pos)))

(defn valid-dir-positions [w h dir-iterator]
  (->> dir-iterator
       (take-while (fn [[x y]] (valid? w h x y)))))

(def find-offsets
  (memoize (fn [w h x y]
             (->> directions
                  (map #(valid-dir-positions w h (dir-iterator % [x y])))
                  (filter #(not (empty? %)))))))

(defn find-neighborhood [selectfn w h x y layout]
  (->> (find-offsets w h x y)
       (map #(map (fn [[ox oy]] (find-seat layout ox oy)) %))
       (map selectfn)))

(defn apply-rule [selectfn min-adjacent w h x y layout]
  (let [seat (find-seat layout x y)
        n (find-neighborhood selectfn w h x y layout)
        occupied (count (filter #(= \# %) n))]
    (cond
      (and (= \L seat) (= 0 occupied)) \#
      (and (= \# seat) (>= occupied min-adjacent)) \L
      :else seat)))

(defn apply-rules [selectfn min-adjacent layout]
  (let [w (count (first layout))
        h (count layout)]
    (->> (for [y (range 0 h)]
           (mapv #(apply-rule selectfn min-adjacent w h % y layout) (range 0 w)))
         (vec))))

(defn rounds [selectfn min-adjacent layout]
  (drop 1 (iterate #(apply-rules selectfn min-adjacent %) layout)))

(defn stabilized [rounds]
  (->> rounds
       (reduce (fn [prev next] (if (= prev next) (reduced next) next)))))

(defn num-occupied [layout]
  (let [freqs (frequencies (flatten layout))]
    (get freqs \#)))

(defn solve1 []
  (->> (parse-layout "resources/day11.txt")
       (rounds first 4)
       (stabilized)
       (num-occupied)))

(defn solve2 []
  (->> (parse-layout "resources/day11.txt")
       (rounds first-visible-seat 5)
       (stabilized)
       (num-occupied)))
