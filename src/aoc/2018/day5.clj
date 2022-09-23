(ns aoc.2018.day5
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 5: Alchemical Reduction ---

(def default-input "src/aoc/2018/day5.txt")
(def test-input "src/aoc/2018/day5test.txt")

(defn- parse
  [filename]
  (slurp filename))

(defn- react?
  [c1 c2]
  (= 32 (Math/abs (- (int c1) (int c2)))))

(defn- do-reaction
  [s]
  (loop [i 1]
    (when (< i (count s))
      (if (react? (get s i) (get s (dec i)))
        (str (subs s 0 (dec i))
             (subs s (inc i)))
        (recur (inc i))))))

(defn- reactions
  [s]
  (->> s (iterate do-reaction) (take-while #(not= % nil))))

(defn- solve1
  [input]
  (->> input reactions last count))

; --- Part Two ---

(defn- solve2
  [input]
  (->> (range 65 91)
       (map (fn [n] (remove #(or (= % (char n))
                                 (= % (char (+ n 32))))
                            input)))
       (map (partial apply str))
       (map solve1)
       (apply min)))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))