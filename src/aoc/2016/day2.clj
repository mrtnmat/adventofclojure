(ns aoc.2016.day2
  (:require [aoc.utils :as aoc]))

(def default-input "src/aoc/2016/day2.txt")
(def test-input "src/aoc/2016/day2test.txt")

(def directions {:up [0 1] :down [0 -1] :right [1 0] :left [-1 0]})
(def char-to-dir {\U :up \D :down \R :right \L :left})


(defn- parse-line
  [line]
  (map #(get char-to-dir %) (seq line)))

(defn- parse
  [filename]
  (map parse-line (aoc/parse-lines filename)))

; part1

(def keypad [[7 8 9]
             [4 5 6]
             [1 2 3]])

(defn- key-num
  [[x y]]
  ((keypad (inc y)) (inc x)))

(defn- legal-pos?
  [pos]
  (> 2 (last (sort (map #(Math/abs %) pos)))))

(defn- move
  [pos dir]
  (let [new-pos (map + pos (dir directions))]
    (if (legal-pos? new-pos) new-pos pos)))

(defn- compute-movements
  ([pos [m & ms]]
   (if m (recur (move pos m)
                ms)
       pos)))

(defn- solution-coords
  [input]
  (loop [pos [0 0]
         ans (transient [])
         [m & ms] input]
    (let [new-pos (compute-movements pos m)]
      (if m (recur new-pos
                   (conj! ans new-pos)
                   ms)
          (persistent! ans)))))

;part2

(def keypad2 [[nil nil \D nil nil]
              [nil \A \B \C nil]
              [5 6 7 8 9]
              [nil 2 3 4 nil]
              [nil nil 1 nil nil]])

(defn- key-num2
  [[x y]]
  ((keypad2 (+ y 2)) (+ x 2)))

(defn- legal-pos2?
  [pos]
  (<= (reduce + 0 (map #(Math/abs %) pos))
      2))

(defn- move2
  [pos dir]
  (let [new-pos (map + pos (dir directions))]
    (if (legal-pos2? new-pos) new-pos pos)))

(defn- compute-movements2
  ([pos [m & ms]]
   (if m (recur (move2 pos m)
                ms)
       pos)))

(defn- solution-coords2
  [input]
  (loop [pos [-2 0]
         ans (transient [])
         [m & ms] input]
    (let [new-pos (compute-movements2 pos m)]
      (if m (recur new-pos
                   (conj! ans new-pos)
                   ms)
          (persistent! ans)))))

(defn- solver1
  [input]
  (map key-num (solution-coords input)))

(defn- solver2
  [input]
  (map key-num2 (solution-coords2 input)))

(def solve1 (aoc/solver {:f solver1
                         :default default-input
                         :parse parse}))

(def solve2 (aoc/solver {:f solver2
                         :default default-input
                         :parse parse}))