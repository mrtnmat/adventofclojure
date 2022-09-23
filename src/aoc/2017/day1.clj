(ns aoc.2017.day1
  (:require [clojure.string :as str]))

(def input-path
  "String containing the path to the folder of the input files"
  "src/aoc/2017/")

(defn- parse
  [filepath]
  (let [data (slurp filepath)]
    (vec (map #(Integer/parseInt %1)
              (str/split data #"")))))

(defn- rotate
  "Returns the sequence of the elements of the vector rotated right by n.
  Negative numbers rotate left."
  ([v]
   (rotate v 1))
  ([v n]
   (let [size (count v)
         steps (mod n size)]
     (concat (subvec v (- size steps))
             (subvec v 0 (- size steps))))))
  

(defn solve1
  ([] (solve1 (str/join [input-path "day1.txt"])))
  ([input]
   (if (string? input)
     (recur (parse input))
     (reduce + 0
             (map (fn [[x y]] (if (= x y) x 0))
                  (map list
                       input
                       (rotate input)))))))

(defn solve2
  ([] (solve2 (str/join [input-path "day1.txt"])))
  ([input]
   (if (string? input)
     (recur (parse input))
     (reduce + 0
             (map (fn [[x y]] (if (= x y) x 0))
                  (map list
                       input
                       (rotate input (/ (count input) 2))))))))