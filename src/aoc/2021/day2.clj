(ns aoc.2021.day2
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 2: Dive! ---

(def default-input "src/aoc/2021/day2.txt")
(def test-input "src/aoc/2021/day2test.txt")

(defn- parse-line
  [s]
  (let [spl (str/split s #" ")
        dir (case (spl 0) "forward" :forward "down" :down "up" :up nil)
        steps (aoc/parse-int (spl 1))]
    [dir steps]))

(defn- parse
  [filename]
  (->> filename
       aoc/parse-lines
       (map parse-line)))

(defn- move
  [[pos depth] [dir steps]]
  (case dir
    :forward [(+ pos steps) depth]
    :up [pos (- depth steps)]
    :down [pos (+ depth steps)]))

(defn- solve1
  [input]
  (let [final-pos (reduce move [0 0] input)]
    (* (final-pos 0) (final-pos 1))))

; --- Part Two ---

(defn- move2
  [[pos depth aim] [dir steps]]
  (case dir
    :forward [(+ pos steps) (+ depth (* aim steps)) aim]
    :up [pos depth (- aim steps)]
    :down [pos depth (+ aim steps)]))

(defn- solve2
  [input]
  (let [final-pos (reduce move2 [0 0 0] input)]
    (* (final-pos 0) (final-pos 1))))

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))