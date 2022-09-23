(ns aoc.2020.day3
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 3: Toboggan Trajectory ---

(def default-input "src/aoc/2020/day3.txt")
(def test-input "src/aoc/2020/day3-test.txt")

(defn- parse-line
  "..##......."
  [s]
  (-> (str "[" s "]")
      (str/replace #"\." "0 ")
      (str/replace #"#" "1 ")
      read-string
      ))

(defn- parse
  [filename]
  (->> filename aoc/slurp-lines (map parse-line)))

(defn- find-trees
  [input [right down]]
  (map #(nth (cycle %1) %2)
       (take-nth down input) (take-nth right (range))))

(defn- solve1
  [input]
  (apply +
         (find-trees input [3 1])))

; --- Part Two ---

(defn- solve2
  [input]
  (apply * (map
            #(apply + (find-trees input %))
            '([1 1] [3 1] [5 1] [7 1] [1 2]))))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))