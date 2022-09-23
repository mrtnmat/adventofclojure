(ns aoc.2021.day1
  (:require [aoc.utils :as aoc]))

;--- Day 1: Sonar Sweep ---

(def ex1 [199 200 208 210 200 207 240 269 260 263])

(def default-input "src/aoc/2021/day1.txt")

(defn parse
  [filename]
  (map #(Integer/parseInt %) (aoc/parse-lines filename)))

(defn count-increments
  ([xs] (count-increments xs 1))
  ([xs n]
   (loop [acc 0
          [head & tail] (map vector
                             xs
                             (drop n xs))]
     (if (empty? tail) acc
         (if (< (head 0) (head 1))
         (recur (inc acc) tail)
         (recur acc tail))))))

(defn solver1
  [input]
  (count-increments input))

(defn solver2
  [input]
  (count-increments input 3))

(def solve1 (aoc/solver {:f solver1
                         :default default-input
                         :parse parse}))

(def solve2 (aoc/solver {:f solver2
                         :default default-input
                         :parse parse}))

;jabjab

(defn count-increments-jab-original [xs]
  (loop [acc 0
         x (first xs)
         l (rest xs)]
    (if (empty? l) acc
        (if
         (< x (first l)) (recur (inc acc) (first l) (rest l))
         (recur acc (first l) (rest l))))))

(defn sliding-window [n xs]
  (loop [window (vec (take n xs))
         tail (drop n xs)
         cursors []]
    (if (= (count tail) 0) (conj cursors window)
        (recur
         (conj (vec (rest window)) (first tail))
         (rest tail)
         (conj cursors window)))))

(defn jab2 [input]
  (->>
   (sliding-window 3 input)
   (map #(apply + %))
   (count-increments-jab-original)))

(defn count-increments-jab
  "jabberabbe's solution extended to part 2."
  ([xs] (count-increments-jab xs 1))
  ([xs n]
   (loop [acc 0
          [head & tail] xs]
     (if (<= (count tail) (dec n)) acc
         (if
          (> (first (drop (dec n) tail)) head)
           (recur (inc acc) tail)
           (recur acc tail))))))