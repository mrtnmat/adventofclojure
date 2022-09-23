(ns aoc.utils
  (:require [clojure.java.io :as io]))

(defn first-duplicate
  "Returns the first duplicate element from a collection."
  [coll]
  (loop [head (first coll)
         tail (rest coll)
         acc (transient #{})]
    (if (contains? acc head)
      head
      (recur (first tail)
             (rest tail)
             (conj! acc head)))))

(defn first-nplicate
  "Returns the first element that appears at least n times from a collection."
  [coll n]
  (loop [head (first coll)
         tail (rest coll)
         acc (transient {})]
    (let [v (inc (acc head 0))]
      (if (= v n)
        head
        (recur (first tail)
               (rest tail)
               (assoc! acc head v))))))

(defn slurp-lines
  "Returns a seq containing each line of text from the file."
  [filepath]
  (with-open [rdr (io/reader filepath)]
    (doall (line-seq rdr))))

(defn parse-lines
  "Returns a seq containing each line of text from the file."
  [filepath]
  (slurp-lines filepath))

(defn parse-int [s] (Integer/parseInt s))



(defn solver
  "Returns a solver function for the given solving fn and input."
  [{f :f
    default :default
    parse :parse}]
  (fn solve
    ([] (solve default))
    ([input]
     (if (string? input)
       (solve (parse input))
       (f input)))))

(defn manh
  "Returns the Manhattan distance between two points."
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn trace "Prints and returns an arg" [arg] (println arg) arg)