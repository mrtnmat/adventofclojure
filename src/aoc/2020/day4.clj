(ns aoc.2020.day4
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 4: Passport Processing ---

(def default-input "src/aoc/2020/day4.txt")
(def test-input "src/aoc/2020/day4test.txt")

(defn- parse-passport
  [passport]
  (let [parsed (re-seq #"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([^\s]+)" passport)]
    (reduce
     (fn
       [acc [_ type value]]
       (assoc acc
              (keyword type)
              value))
     {}
     parsed)))

(defn- valid?
  [passport-data]
  (if (= (->> passport-data
              keys
              (remove #(= :cid %))
              (count))
         7)
    true
    false))

(defn- parse
  [filename]
  (let [system-newline (with-out-str (newline))]
    (as-> filename $
      (slurp $)
      (str/split $ (re-pattern (str system-newline system-newline)))
      (map parse-passport $))))

(defn- solve1
  [input]
  (->> input (filter valid?) count))

; --- Part Two ---

(defn- in-range?
  [n min max]
  (and (>= n min) (<= n max)))

(defn- valid-hgt?
  [hgt]
  (let [[_ v unit] (first (re-seq #"(\d+)(.+)" hgt))
        v (aoc/parse-int v)]
    (case unit
      "cm" (in-range? v 150 193)
      "in" (in-range? v 59 76)
      false)))

(defn- valid-hcl?
  [hcl]
  (->> hcl (re-seq #"^\#[0-9a-f]{6}$") first string?))

(def valid-ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn- valid-pid?
  [pid]
  (and (= 9 (count pid))
       (try (int? (aoc/parse-int pid))
            (catch Exception _ false))))

(defn- valid2?
  [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and (in-range? (aoc/parse-int byr) 1920 2002)
       (in-range? (aoc/parse-int iyr) 2010 2020)
       (in-range? (aoc/parse-int eyr) 2020 2030)
       (valid-hgt? hgt)
       (valid-hcl? hcl)
       (contains? valid-ecl ecl)
       (valid-pid? pid)))

(defn- solve2
  [input]
  (->> input (filter valid?) (filter valid2?)))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))