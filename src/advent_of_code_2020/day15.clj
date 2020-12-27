(ns advent-of-code-2020.day15)

(defn speak [spoken turn last]
  (let [last-turn (get spoken last)]
    (if (nil? last-turn)
      0
      (- turn last-turn))))

(defn solve [n]
  (loop [spoken {0 1, 13 2, 1 3, 8 4, 6 5}
         turn 6
         last 15]
    (if (= n turn)
      last
      (recur (assoc spoken last turn)
             (inc turn)
             (speak spoken turn last)))))

(defn solve1 []
  (solve 2020))

(defn solve2 []
  (solve 30000000))
