(ns aoc.2015.day2
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

(def default-input "src/aoc/2015/day2.txt")

(def ex1 [2 3 4])

(defn- parse
  [filename]
  (map (fn [line]
         (sort (map #(Integer/parseInt %)
                    (str/split line #"x"))))
       (aoc/parse-lines filename)))

(defn- rrprism-areas
  [[x y z]]
  [(* x y) (* x z) (* z y)])

(defn- paper-required
  [sides]
  (let [areas (rrprism-areas sides)]
    (+ (first areas)
       (* 2 (reduce
             + 0 areas)))))

(defn- ribbon-required
  [[x y z]]
  (+ (* x y z)
     (* 2 (+ x y))))

(defn- solver1
  [input]
  (reduce + 0
          (map paper-required input)))

(defn- solver2
  [input]
  (reduce + 0
          (map ribbon-required input)))

(def solve1 (aoc/solver {:f solver1
                         :default default-input
                         :parse parse}))

(def solve2 (aoc/solver {:f solver2
                         :default default-input
                         :parse parse}))