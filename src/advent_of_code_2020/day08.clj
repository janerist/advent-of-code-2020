(ns advent-of-code-2020.day08
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[_ i v] (re-matches #"(acc|jmp|nop) ([+\-]\d+)" line)]
    [i (Integer/parseInt v)]))

(defn execute [{pc :pc acc :acc} [instr value]]
  (case instr
    "acc" {:pc (inc pc) :acc (+ acc value)}
    "jmp" {:pc (+ pc value) :acc acc}
    "nop" {:pc (inc pc) :acc acc}))

(defn run-program [program]
  (loop [{pc :pc :as state} {:pc 0 :acc 0}
         history #{}]
    (cond
      (contains? history pc) [state :infinite-loop]
      (= pc (count program)) [state :success]
      :else (let [next (program pc)
                  result (execute state next)]
              (recur result (conj history pc))))))

(defn solve1 []
  (->> (slurp "resources/day08.txt")
       (str/split-lines)
       (mapv parse-instruction)
       (run-program)
       (first)
       :acc))

(defn run-program-with-replacement [program idx replacement]
  (let [new-instructions (assoc program idx replacement)]
    (run-program new-instructions)))

(defn solve2 []
  (let [program (->> (slurp "resources/day08.txt")
                          (str/split-lines)
                          (mapv parse-instruction))
        jmps-n-nops (filter
                      (fn [[_ [i _]]] (or (= i "jmp") (= i "nop")))
                      (map-indexed (fn [idx i] [idx i]) program))]
    (->> jmps-n-nops
         (map (fn [[idx [i v]]]
                (run-program-with-replacement
                  program
                  idx
                  (case i "jmp" ["nop" v] "nop" ["jmp" v]))))
         (filter (fn [[_ term]] (= term :success)))
         (first)
         (first)
         :acc)))
