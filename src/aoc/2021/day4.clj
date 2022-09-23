(ns aoc.2021.day4
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 4: Giant Squid ---

(def default-input "src/aoc/2021/day4.txt")
(def test-input "src/aoc/2021/day4test.txt")

(defn- parse-row
  [row]
  (as-> row $
    (str/trim $)
    (str/split $ #"\s+")
    (map aoc/parse-int $)
    (map #(vector % false) $)))

(defn- parse-board
  [[_ & rows]]
  (map parse-row rows))

(defn- parse-nums
  [nums]
  (as-> nums $
    (str/split $ #",")
    (map aoc/parse-int $)))

(defn- parse
  [filename]
  (let [[nums & boards] (aoc/slurp-lines filename)
        boards (partition 6 boards)]
       {:nums (parse-nums nums) :boards (map parse-board boards)}))

(defn- mark-number
  [n row]
  (map #(if (= n (first %)) [n true] %) row))

(defn- mark-all-boards
  [n boards]
  (map #(map (partial mark-number n) %) boards))

(defn- bingo?
  [row]
  ((complement #(some (fn [[_ x]] (false? x)) %)) row))

(defn- bingo-board?
  [board]
  (or (some true? (map bingo? board))
      (some true? (apply map #(bingo? (list %1 %2 %3 %4 %5)) board))))

(defn- compute-answer
  [[n board]]
  (->> board
       (apply concat)
       (filter (fn [[_ v]] (= v false)))
       (map first)
       (reduce +)
       (* n)))

(defn- solve1
  [input]
  (->> (reductions #(mark-all-boards %2 %1) (:boards input) (:nums input))
       (drop 1)
       (map #(filter bingo-board? %))
       (map #(vector %1 (first %2)) (:nums input))
       (drop-while #(empty? (second %)))
       first
       compute-answer))

; --- Part Two ---

(defn- compute-answer2
  [[n board]]
  (as-> board $
    (apply concat $)
    (filter (fn [[_ v]] (= v false)) $)
    (map first $)
    (reduce + $)
    (- $ n)
    (* $ n)
    ))

(defn- solve2
  [input]
  (->> (reductions #(mark-all-boards %2 %1) (:boards input) (:nums input))
       (map #(remove bingo-board? %))
       (map #(vector %1 (first %2)) (:nums input))
       (take-while #(seq? (second %)))
       last
       compute-answer2))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))