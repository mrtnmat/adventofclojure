(ns aoc.2020.day1
  (:require [aoc.utils :as aoc]))

(def input-file "src/aoc/2020/day1.txt")
(def ex1 [1721 979 366 299 675 1456])

(defn find-complement
  ([coll]
   (find-complement coll 2020))
  ([[x & xs] n]
   (when xs
     (if (some #(= (+ x %) n) xs)
       x
       (recur xs n)))))

(defn- parse
  [filename]
  (map
   #(Integer/parseInt %)
   (aoc/parse-lines filename)))

(defn- solver1
  [input]
  (let [v (find-complement input)]
    (* v (- 2020 v))))

(defn- solver2
  [[x & xs]]
  (let [dif (- 2020 x)
        v (find-complement xs dif)]
    (if v
      (* x v (- dif v))
      (recur xs))))

(def solve1 (aoc/solver {:f solver1
                         :data input-file
                         :parse parse}))
(def solve2 (aoc/solver {:f solver2
                         :data input-file
                         :parse parse}))