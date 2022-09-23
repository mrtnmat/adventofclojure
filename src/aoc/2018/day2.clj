(ns aoc.2018.day2
  (:require [aoc.utils :as aoc]))

; --- Day 2: Inventory Management System ---

(def default-input "src/aoc/2018/day2.txt")
(def test-input "src/aoc/2018/day2-test.txt")

(defn- parse
  [filename]
  (aoc/parse-lines filename))

(defn- f1
  [lines]
  (loop [[x & xs] lines
         a 0
         b 0]
    (if x
      (let [freq (vals (frequencies x))]
        (recur
         xs
         (if (some #(= % 2) freq) (inc a) a)
         (if (some #(= % 3) freq) (inc b) b)))
      (* a b))))

(defn- solver1
  [input]
  (f1 input))

; part2

(defn- count-diff
  [s1 s2]
  (remove nil?
          (map (fn [a b] (if (= a b) a nil))
               s1 s2)))

(defn- solver2
  [[x & xs]]
  (when-let [tail (seq xs)]
    (let [res (->> xs
                 (map (partial count-diff x))
                 (filter #(= (count %) (dec (count x))))
                 first)]
      (if res
        (apply str res)
        (solver2 tail)))))

(def solve1 (aoc/solver {:f solver1
                         :default default-input
                         :parse parse}))

(def solve2 (aoc/solver {:f solver2
                         :default default-input
                         :parse parse}))