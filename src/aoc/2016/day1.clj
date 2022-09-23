(ns aoc.2016.day1
  (:require [clojure.string :as str]))

; input -> seq movements -> final position -> final x + y

(def compass-points
  "Vector of compass points."
  [:north :east :south :west])
(def start
  "Starting position."
  {:pos {:x 0 :y 0} :facing :north})
(def input-path
  "String containing the path to the folder of the input files"
  "src/aoc/2016/")

(defn- parse-movement
  "Parse a string and returns the corresponding movement."
  [s]
  {:rel-dir (case (first s)
              \L :left
              \R :right)
   :steps (Integer/parseInt (subs s 1))})

(defn- parser
  "Returns a lazy seq containing every movement needed 
  to reach the Easter Bunny Headquarters 
  for a given valid input file."
  [path]
  (let [inp (str/split (slurp path) #", ")]
    (map parse-movement inp)))

(defn- mov-vector
  "Returns the movement vector for the corresponding movement."
  [dir steps]
  (case dir
    :north {:x 0, :y steps}
    :east {:x steps, :y 0}
    :south {:x 0, :y (- steps)}
    :west {:x (- steps), :y 0}))

(defn- pos-sum
  "Returns the position resulting from the sum of two positions."
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  {:x (+ x1 x2) :y (+ y1 y2)})

(defn- turn
  "Given a cardinal point and a relative direction 
  returns the new cardinal point after turning toward that direction."
  [dir rel-dir]
  (let [turnStep (case rel-dir
                   :right 1
                   :left 3)
        index (.indexOf compass-points dir)]
    (compass-points (mod (+ index turnStep) 4))))

(defn- move
  "Returns the position after a movement from a given position."
  [{pos :pos facing :facing} {dir :rel-dir steps :steps}]
  (let [new-dir (turn facing dir)]
    {:pos (pos-sum pos (mov-vector new-dir steps))
     :facing new-dir}))

(defn- bunnyhq-path
  "Returns a lazy seq of every point on the path 
  to the Easter Bunny Headquarters."
  [start-pos movements]
  (reductions
   move
   start-pos
   movements))

(defn- manh-dist
  "Returns the Manhattan distance between two points."
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn solve1
  "Returns the solution for part1."
  [filename]
  (let [path (map
              #(:pos %)
              (bunnyhq-path start
               (parser (str/join [input-path filename]))))]
    (manh-dist (:pos start)
               (last path))))


;Part2

(defn- step
  "Take one step forward"
  [{pos :pos dir :facing}]
  {:pos (pos-sum pos (mov-vector dir 1))
   :facing dir})

(defn- segment
  "Returns the seq of all the points between two co-linear points."
  [{pos :pos facing :facing} {rel-dir :rel-dir steps :steps}]
  (let [new-dir (turn facing rel-dir)]
    (take steps (drop 1 (iterate step {:pos pos :facing new-dir})))))

(defn- first-duplicate
  "Returns the first duplicate element from a sorted collection."
  [coll]
  (loop [xs (map #(:pos %) coll)
         acc (transient #{})]
    (if (contains? acc (first xs))
      (first xs)
      (recur (rest xs)
             (conj! acc (first xs))))))

(defn- first-pos-visited-twice
  "Returns the first positition visited twice."
  [filename]
  (let [turns (parser (str/join [input-path filename]))
        path (reduce #(flatten (cons %1 (segment (last %1) %2)))
                     [start] turns)]
    (first-duplicate path)))

(defn solve2
  "Returns the Manhattan distance of the first
  positition visited twice from the origin."
  [filename]
  (manh-dist (:pos start) (first-pos-visited-twice filename)))

     