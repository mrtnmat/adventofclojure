(ns aoc.2019.day3
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 3: Crossed Wires ---

(def default-input "src/aoc/2019/day3.txt")
(def test-input "src/aoc/2019/day3-test.txt")

(def cardinal-points {:U [0 1]
                      :D [0 -1]
                      :R [1 0]
                      :L [-1 0]})

(defn- parse-line
  [s]
  (let [parse-movement #(read-string (str "[:" (subs % 0 1) " " (subs % 1) "]"))
        expand-movement (fn [[dir steps]] (repeat steps (cardinal-points dir)))
        vector-sum (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])]
    (->> (str/split s #",")
         (map parse-movement)
         (mapcat expand-movement)
         (reductions vector-sum [0 0]))))

(defn- parse
  [filename]
  (->> filename aoc/slurp-lines (map parse-line)))

(defn- intersections
  [wire1 wire2]
  (let [first-wire (set wire1)]
    (->> wire2
         (filter #(contains? first-wire %)))))

(defn- solve1
  [input]
  (->> (intersections (first input) (second input))
       (map #(aoc/manh [0 0] %))
       (sort)
       (second)))

; --- Part Two ---

(defn- solve2
  [input]
  (let [wire1 (first input)
        wire2 (second input)
        xsec (intersections wire1 wire2)]
    (->> (map + (map #(.indexOf wire1 %) xsec) (map #(.indexOf wire2 %) xsec))
         (sort)
         (second))))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))