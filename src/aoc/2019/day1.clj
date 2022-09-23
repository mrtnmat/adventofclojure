(ns aoc.2019.day1
  (:require [aoc.utils :as aoc]))

(def input-file "src/aoc/2019/day1.txt")

(defn- parse
  [filepath]
  (map
   #(Integer/parseInt %)
   (aoc/parse-lines filepath)))

(defn- fuel
  [mass]
  (let [fuel (-
              (quot mass 3)
              2)]
  (if (< fuel 0)
    0
    fuel)))

(defn- fuel2
  [mass]
  (reduce + 0
          (take-while
           pos?
           (rest (iterate fuel mass)))))

(defn- solver1
  [input]
  (reduce + 0
          (map
           fuel
           input)))

(defn- solver2
  [input]
  (reduce + 0
          (map
           fuel2
           input))
  )

(def solve1 (aoc/solver {:f solver1
                         :data input-file
                         :parse parse}))
(def solve2 (aoc/solver {:f solver2
                         :data input-file
                         :parse parse}))