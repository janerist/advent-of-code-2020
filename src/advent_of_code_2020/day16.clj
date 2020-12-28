(ns advent-of-code-2020.day16
  (:require [clojure.string :as str]))

(defn parse-field [field]
  (let [[_ name s1 e1 s2 e2] (re-matches #"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)" field)]
    [name
     [[(Integer/parseInt s1)
       (Integer/parseInt e1)]
      [(Integer/parseInt s2)
       (Integer/parseInt e2)]]]))

(defn parse-fields [fields]
  (let [lines (str/split fields #"\n")]
    (into {} (map parse-field lines))))

(defn parse-ticket [ticket]
  (mapv #(Integer/parseInt %) (str/split ticket #",")))

; {:fields {"departure location" [[47 874] [885 960]]
;           "departure station" [[25 616] [622 964]]
;           ...}
;  :ticket [83 137 101 73 67 ...]
;  :nearby-tickets [[191 477 199 ...]
;                   [598 628 446 ...]]}
(defn parse-input [input]
  (let [[fields ticket nearby-tickets] (str/split input #"\n\n")]
    {:fields         (parse-fields fields)
     :ticket         (let [[_ numbers] (str/split-lines ticket)]
                       (parse-ticket numbers))
     :nearby-tickets (let [[_ numbers] (str/split nearby-tickets #"\n" 2)]
                       (mapv parse-ticket (str/split-lines numbers)))}))


; Is the number valid for the field?
(defn valid? [[[s1 e1] [s2 e2]] number]
  (or
    (<= s1 number e1)
    (<= s2 number e2)))

; Is the number valid for any field?
(defn valid-number? [fields number]
  (->> (vals fields)
       (some #(valid? % number))
       (some?)))

; Are every number in the ticket valid?
(defn valid-ticket? [fields ticket]
  (every? #(valid-number? fields %) ticket))

(defn solve1 []
  (let [input (slurp "resources/day16.txt")
        {fields :fields nearby-tickets :nearby-tickets} (parse-input input)]
    (->> nearby-tickets
         (flatten)
         (filter #(not (valid-number? fields %)))
         (apply +))))

;-----------------------------------------------------------------------------------

; Which fields are the numbers valid for?
(defn valid-for? [fields numbers]
  (filter (fn [[_ v]] (every? #(valid? v %) numbers)) fields))

; Returns the possible fields for each position in tickets
(defn candidate-fields [fields tickets]
  (for [pos (range 0 (count (first tickets)))]
    (->> tickets
         (map #(nth % pos))
         (valid-for? fields)
         (mapv first))))

; Reduces the candidate fields to one field per pos
(defn find-field-order [candidate-fields]
  (loop [c candidate-fields]
    (if (every? #(not (vector? %)) c)
      c
      (let [next (first (first (filter #(= 1 (count %)) c)))]
        (recur (map (fn [fields]
                      (if (vector? fields)
                        (if (> (count fields) 1)
                          (vec (remove #(= next %) fields))
                          (first fields))
                        fields)) c))))))

(defn solve2 []
  (let [input (slurp "resources/day16.txt")
        {fields :fields ticket :ticket nearby-tickets :nearby-tickets} (parse-input input)
        valid-tickets (filter #(valid-ticket? fields %) nearby-tickets)
        field-order (find-field-order (candidate-fields fields valid-tickets))]
    (->> (zipmap field-order ticket)
         (filter (fn [[k _]] (.startsWith k "departure")))
         (vals)
         (apply *))))


