(ns aoc.2017.day4
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 4: High-Entropy Passphrases ---

(def default-input "src/aoc/2017/day4.txt")
(def test-input "src/aoc/2017/day4-test.txt")

(defn- parse-line
  "aa bb cc dd ee" ;["aa" "bb" "cc" "dd" "ee"]
  [s]
  (str/split s #" "))

(defn- parse
  [filename]
  (->> filename aoc/slurp-lines (map parse-line)))

(defn- solve1
  [input]
  (->> input
       (map (comp vals frequencies))
       (remove (fn [a] (some #(> % 1) a)))
       (count)))

; --- Part Two ---

(defn- solve2
  [input]
  (->> input
       (map (comp vals frequencies (partial map frequencies)))
       (remove (fn [a] (some #(> % 1) a)))
       (count)))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))