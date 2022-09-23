(ns aoc.2017.day3
  (:require [aoc.utils :as aoc]))

; --- Day 3: Spiral Memory ---

(defn- step1
  [[x y] dir]
  (case dir
    :north [x (inc y)]
    :south [x (dec y)]
    :east [(inc x) y]
    :west [(dec x) y]))

(defn- manh [[x y]] (+ (Math/abs x) (Math/abs y)))

(defn- spiral-coord []
  (let [turn-left (cycle '(:east :north :west :south))
        spiral-pattern (interleave (iterate inc 1) (iterate inc 1))]
    (->> turn-left
         (map #(repeat %1 %2) spiral-pattern)
         (apply concat)
         (reductions step1 [0 0]))))

(defn- solve1
  [input]
  (->> (spiral-coord) (take input) (last) (manh)))

(def solver1 (aoc/solver {:f solve1
                          :default 277678
                          :parse nil}))

; --- Part Two ---

(defn- step2
  [[x y] dir]
  (case dir
    :ne [(inc x) (inc y)]
    :nw [(inc x) (dec y)]
    :se [(dec x) (inc y)]
    :sw [(dec x) (dec y)]
    (:north :south :east :west) (step1 [x y] dir)))

(def all-dir [:north :south :west :east :ne :nw :se :sw])

(defn- solve2
  [n]
  (loop [[x & xs] (drop 1 (spiral-coord))
         acc (transient {[0 0] 1})]
    (let [new-val (apply +
                         (map
                          #(as-> % $ (step2 x $) (acc $ 0))
                          all-dir))]
      (if (< new-val n)
        (recur xs
               (assoc! acc x new-val))
        new-val))))

(def solver2 (aoc/solver {:f solve2
                          :default 277678
                          :parse nil}))