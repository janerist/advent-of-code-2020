(ns advent-of-code-2020.day12
  (:require [clojure.string :as str]))

(defn parse-instructions [input]
  (->> input
       (str/split-lines)
       (map (fn [[action & numbers]]
              [action (Integer/parseInt (apply str numbers))]))))

(defn apply-instruction [[east north dir] [action val]]
  (case action
    \N [east (+ north val) dir]
    \S [east (- north val) dir]
    \E [(+ east val) north dir]
    \W [(- east val) north dir]
    \L [east north (mod (- dir val) 360)]
    \R [east north (mod (+ dir val) 360)]
    \F (apply-instruction [east north dir] (case dir
                                             0 [\N val]
                                             90 [\E val]
                                             180 [\S val]
                                             270 [\W val]))))

(defn solve1 []
  (let [instructions (parse-instructions (slurp "resources/day12.txt"))
        boat [0 0 90]
        [east north _] (reduce apply-instruction boat instructions)]
    (+ (Math/abs east) (Math/abs north))))

(defn apply-instruction2 [[boat waypoint] [action val]]
  (let [[be bn] boat
        [we wn] waypoint]
    (case action
      \N [boat [we (+ wn val)]]
      \S [boat [we (- wn val)]]
      \E [boat [(+ we val) wn]]
      \W [boat [(- we val) wn]]
      \L [boat (case (mod val 360)
                 90 [(- wn) we]
                 180 [(- we) (- wn)]
                 270 [wn (- we)])]
      \R [boat (case (mod val 360)
                 90 [wn (- we)]
                 180 [(- we) (- wn)]
                 270 [(- wn) we])]
      \F [[(+ be (* we val)) (+ bn (* wn val))] waypoint])))

(defn solve2 []
  (let [instructions (parse-instructions (slurp "resources/day12.txt"))
        boat [0 0]
        waypoint [10 1]
        [[east north] _] (reduce apply-instruction2 [boat waypoint] instructions)]
    (+ (Math/abs east) (Math/abs north))))
