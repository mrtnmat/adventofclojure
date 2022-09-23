(ns aoc.2020.day2
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 2: Password Philosophy ---

(def default-input "src/aoc/2020/day2.txt")

(defn- inrange [min max n]
  (and (<= min n) (>= max n)))

(defn- validate
  [[low high c pwd]]
  (->> pwd
       (filter (partial = (first c)))
       (count)
       (inrange low high)))

(defn- parse-line [s]
  (read-string
   (str
    "["
    (-> s
        (str/replace #"[ :-]+" " ")
        (str/replace #"([a-z]+)" "\"$1\""))
    "]")))

(defn- parse
  ([] (parse default-input))
  ([input] (->> input aoc/parse-lines (map parse-line))))

(defn- solve1
  [input]
  (->> input
       (map validate)
       (filter true?)
       count))

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

; --- Part Two ---

(defn- validate2
  [[pos1 pos2 c pwd]]
  (if (not= (= (first c) (get pwd (dec pos1)))
            (= (first c) (get pwd (dec pos2))))
    true
    false))

(defn- solve2
  [input]
  (->> input
       (map validate2)
       (filter true?)
       count))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))