(ns aoc.2021.day3
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 3: Binary Diagnostic ---

(def default-input "src/aoc/2021/day3.txt")
(def test-input "src/aoc/2021/day3-test.txt")

(defn- parse [filename]
  (->> filename
       (aoc/slurp-lines)
       (map #(str "[" %1 "]"))
       (map #(str/replace % #"" " "))
       (map #(str/replace % #"0" "-1"))
       (map read-string)))

(defn- bin-to-dec [bin] (->> bin
                             (reverse)
                             (map #(* %1 %2) (map #(Math/pow 2 %) (range)))
                             (apply +)
                             (int)
                             ))

(defn- binary-inverse [bin] (map #(if (zero? %) 1 0) bin))

(defn- to-bin [x] (map #(if (> % 0) 1 0) x))

(defn- solve1
  [input]
  (let [gamma (to-bin (apply (partial map +) input))
        epsilon (binary-inverse gamma)]
    (* (bin-to-dec gamma) (bin-to-dec epsilon))))

; --- Part Two ---

(defn- flatten-int [n] (case n 0 0 (/ n (Math/abs n))))

(defn- most-common-nth
  [n list]
  (->> list
       (map #(nth % n))
       (apply +)
       flatten-int))

(defn- co2
  [input]
  (loop [i 0
         coll input]
    (if (> (count coll) 1)
      (let [most-common (case (most-common-nth i coll)
                          (1 0) 1
                          -1 -1)]
        (recur
         (inc i)
         (->> coll (filter #(= most-common (% i))))))
      (->> coll first to-bin bin-to-dec))))

(defn- oxy
  [input]
  (loop [i 0
         coll input]
    (if (> (count coll) 1)
      (let [most-common (case (most-common-nth i coll)
                          (1 0) 1
                          -1 -1)]
        (recur
         (inc i)
         (->> coll (remove #(= most-common (% i))))))
      (->> coll first to-bin bin-to-dec))))

(defn- solve2
  [input]
  (* (co2 input) (oxy input)))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))