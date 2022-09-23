(ns aoc.2015.day3
  (:require [aoc.utils :as aoc])
  (:require [clojure.set]))

; --- Day 3: Perfectly Spherical Houses in a Vacuum ---

(def default-input "src/aoc/2015/day3.txt")
(def test-input "src/aoc/2015/day3test.txt")

(defn- move
  [[x y] dir]
  (case dir
    :north [x (+ y 1)]
    :south [x (- y 1)]
    :east [(+ x 1) y]
    :west [(- x 1) y]))

(defn- parse
  [filename]
  (map #(case %
          \^ :north
          \v :south
          \> :east
          \< :west)
       (slurp filename)))

(defn- visited-locations
  [movements]
  (set (reductions move [0 0] movements)))

(defn- solve1
  [input]
  (count (visited-locations input)))

; --- Part Two ---

(defn- solve2
  [input]
  (let [santa (take-nth 2 input)
        robo-santa (take-nth 2 (drop 1 input))]
        (count
         (clojure.set/union
          (visited-locations santa)
          (visited-locations robo-santa)))))

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))