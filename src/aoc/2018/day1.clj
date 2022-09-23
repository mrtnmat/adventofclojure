(ns aoc.2018.day1
  (:require [aoc.utils :as aoc])
  (:require [clojure.java.io :as io]))

(def input-file "src/aoc/2018/day1.txt")

(defn- parse
  [filepath]
  (with-open [rdr (io/reader filepath)]
    (map #(Integer/parseInt %) (doall (line-seq rdr)))))

(defn solver1
  [input]
  (reduce + 0 input))

(defn solver2
  [input]
  (aoc/first-duplicate (reductions + 0 (cycle input))))

(def solve1 (aoc/solver {:f solver1
                         :data input-file
                         :parse parse}))
(def solve2 (aoc/solver {:f solver2
                         :data input-file
                         :parse parse}))