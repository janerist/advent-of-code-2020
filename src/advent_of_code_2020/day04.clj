(ns advent-of-code-2020.day04
  (:require [clojure.string :as str]))

; Passport has 8 fields
; All fields except cid must be present for it to be valid
; Field are key:value separated by space or newline
; Blank lines separate passports

(defn number-between? [min max v]
  (if-let [n (re-matches #"\d+" v)]
    (<= min (Integer/parseInt n) max)))

(def validators
  {:byr (partial number-between? 1920 2002)
   :iyr (partial number-between? 2010 2020)
   :eyr (partial number-between? 2020 2030)
   :hgt (fn [v]
          (if-let [[_ hgt unit] (re-matches #"(\d+)(cm|in)" v)]
            (cond
              (= "cm" unit) (<= 150 (Integer/parseInt hgt) 193)
              (= "in" unit) (<= 59 (Integer/parseInt hgt) 76))))
   :hcl (fn [v]
          (re-matches #"#[0-9a-f]{6}" v))
   :ecl (fn [v]
          (some #(= v %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))
   :pid (fn [v]
          (re-matches #"\d{9}" v))
   :cid (fn [_] true)})

(defn fields-valid? [passport]
  (every? (fn [[k v]] ((k validators) v)) passport))

(defn required-fields-present? [passport]
  (every? #(contains? passport %) [:byr :iyr :eyr :hgt :hcl :ecl :pid]))

(defn parse-passport [raw-passp]
  (->> (str/split raw-passp #"\n| ")
       (map #(str/split % #":"))
       (reduce (fn [m [k v]] (assoc m (keyword k) v)) {})))

(defn solve1 []
  (->> (str/split (slurp "resources/day04.txt") #"\n\n")
       (map parse-passport)
       (filter required-fields-present?)
       (count)))

(defn solve2 []
  (->> (str/split (slurp "resources/day04.txt") #"\n\n")
       (map parse-passport)
       (filter required-fields-present?)
       (filter fields-valid?)
       (count)))
