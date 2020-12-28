(ns advent-of-code-2020.day17
  (:require [clojure.string :as str]))

(defn parse-input [input dims]
  (->> input
       (str/split-lines)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c]
                                     [(vec (concat [x y] (repeat (- dims 2) 0))) c])
                                   line)))
       (apply concat)
       (into {})))

(def find-neighborhood
  (memoize (fn [[x y z w]]
             (for [xo [-1 0 1]
                   yo [-1 0 1]
                   zo [-1 0 1]
                   wo (if (nil? w) [0] [-1 0 1])
                   :let [x' (+ x xo)
                         y' (+ y yo)
                         z' (+ z zo)
                         w' (if (nil? w) 0 (+ w wo))]
                   :when (not (and (= x x') (= y y') (= z z') (or (nil? w) (= w w'))))]
               (if (nil? w)
                 [x' y' z']
                 [x' y' z' w'])))))

(defn active? [v]
  (= \# v))

(defn run-cycle [grid]
  (let [neighbors (set (apply concat (map (fn [[k _]] (find-neighborhood k)) grid)))
        grid (merge (zipmap neighbors (repeat (count neighbors) \.)) grid)]
    (reduce (fn [acc [k v]]
              (let [neighbors (find-neighborhood k)
                    num-active (count (filter #(active? (get grid %)) neighbors))]
                (cond
                  (and (active? v) (not (or (= 2 num-active) (= 3 num-active))))
                  (assoc acc k \.)

                  (and (not (active? v)) (= 3 num-active))
                  (assoc acc k \#)

                  :else (assoc acc k v))))
            {}
            grid)))

(defn solve [dims]
  (->> (parse-input (slurp "resources/day17.txt") dims)
       (iterate run-cycle)
       (drop 6)
       (first)
       (vals)
       (filter #(= \# %))
       (count)))

(defn solve1 []
  (solve 3))

(defn solve2 []
  (solve 4))
