(ns aoc.2017.day5
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 5: A Maze of Twisty Trampolines, All Alike ---

(def default-input "src/aoc/2017/day5.txt")
(def test-input "src/aoc/2017/day5test.txt")

(defn- new-memory
  [{:keys [i mem]}]
  (if (< i (count mem))
    {:i (+ i (get mem i))
     :mem (assoc mem i (inc (get mem i)))}
    {:i i :mem mem}))

(defn- parse
  [filename]
  (->> (aoc/slurp-lines filename)
       (map aoc/parse-int)
       (apply vector)))

(defn- solve1
  [input]
  (->> {:i 0 :mem input}
       (iterate new-memory)
       (take-while #(< (:i %) (count (:mem %))))
       count
       ))

; --- Part Two ---

(defn- new-memory2
  [{:keys [i mem]}]
  (if (< i (count mem))
    (let [offset (get mem i)]
      {:i (+ i offset)
       :mem (assoc mem i (if (>= offset 3)
                           (dec offset)
                           (inc offset)))})
    {:i i :mem mem}))

(defn- solve2
  [input]
  (->> {:i 0 :mem input}
       (iterate new-memory2)
       (take-while #(< (:i %) (count (:mem %))))
       count))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))