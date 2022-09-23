(ns aoc.2015.day5
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 5: Doesn't He Have Intern-Elves For This? ---

(def default-input "src/aoc/2015/day5.txt")
(def test-input "src/aoc/2015/day5test.txt")

(defn- nice?
  [s]
  (and (nil? (re-find #"ab|cd|pq|xy" s))
       (not= nil (re-find #"([a-z])\1+" s))
       (not= nil (re-find #"[aeiou].*[aeiou].*[aeiou]" s))))

(defn- parse
  [filename]
  (aoc/slurp-lines filename))

(defn- solve1
  [input]
  (->> input
       (filter nice?)
       count))

; --- Part Two ---

(defn- nice2?
  [s]
  (and (not= nil (re-find #"([a-z]{2}).*\1" s))
       (not= nil (re-find #"([a-z])[a-z]{1}\1" s))))

(defn- solve2
  [input]
  (->> input
       (filter nice2?)
       count))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))