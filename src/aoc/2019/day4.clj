(ns aoc.2019.day4
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str])
  )

; --- Day 4: Repose Record ---

(def default-input "src/aoc/2019/day4.txt")

(defn- parse-line
  [s]
  (-> (str "[" s "]") (str/replace #"-" " ") read-string))

(defn- parse
  [filename]
  (->> filename slurp parse-line))

(defn- adj-same?
  [pwd]
  (->> pwd (map #(= %1 %2) (drop 1 pwd)) (some true?)))

(defn- valid?
  [pwd]
  (and (->> pwd count (= 6))
       (adj-same? pwd)
       (= pwd (apply str (sort pwd)))))

(defn- solve1
  [input]
  (->> (range (first input) (second input)) (map (comp valid? str)) (filter true?) count))

; --- Part Two ---

(defn- adj-same2?
  [pwd]
  (loop [[x & xs] pwd]
    (when (seq xs)
      (let [var1 (count (take-while #(= % x) xs))]
        (if (= var1 1)
          true
          (recur (drop-while #(= % x) xs)))))))

(defn- valid2?
  [pwd]
  (and (->> pwd count (= 6))
       (adj-same2? pwd)
       (= pwd (apply str (sort pwd)))))

(defn- solve2
  [input]
  (->> (range (first input) (second input)) (map (comp valid2? str)) (filter true?) count))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))