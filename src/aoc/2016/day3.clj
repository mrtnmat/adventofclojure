(ns aoc.2016.day3
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 3: Squares With Three Sides ---

(def default-input "src/aoc/2016/day3.txt")
(def test-input "src/aoc/2016/day3test.txt")

(defn- triangle?
  [[x y z]]
  (and (< z (+ x y))
       (< x (+ y z))
       (< y (+ x z))))

(defn- parse-line
  "483  634  611"
  [s]
  (map aoc/parse-int (str/split (str/trim s) #"\s+")))

(defn- parse
  [filename]
  (->> filename
       aoc/parse-lines
       (map parse-line)))

(defn- solve1
  [input]
  (count
   (filter triangle? input)))

; --- Part Two ---

(defn- parse-3lines
  [[[x1 x2 x3] [y1 y2 y3] [z1 z2 z3]]]
  (list [x1 y1 z1] [x2 y2 z2] [x3 y3 z3]))

(defn- parse2
  [filename]
  (->> filename
       aoc/parse-lines
       (map parse-line)
       (partition 3)
       (map parse-3lines)
       (apply concat)))

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse2}))