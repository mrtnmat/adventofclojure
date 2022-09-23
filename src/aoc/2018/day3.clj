(ns aoc.2018.day3
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as comb]))

; --- Day 3: No Matter How You Slice It ---

(def default-input "src/aoc/2018/day3.txt")
(def test-input "src/aoc/2018/day3-test.txt")

(defn- parse-line
  "#1 @ 7,589: 24x11"
  [s]
  (as-> s $
    (str/replace $ #".+@ " "")
    (str/replace $ #"[,: x]+" " ")
    (str "[" $ "]")
    (read-string $)))

(defn- fabric-points [[from-left from-top w h]]
  (comb/cartesian-product
   (range from-left (+ from-left w))
   (range from-top (+ from-top h))))

(defn- parse
  [filename] (->> filename aoc/parse-lines (map (comp fabric-points parse-line))))

(defn- solve1
  [input]
  (->> input (apply concat) frequencies vals (remove (partial = 1)) count))

; --- Part Two ---

(defn- solve2
  [input]
  (let [freq (frequencies (apply concat input))
        has-overlaps #(->> %
                          (map freq)
                          (some (partial not= 1)))]
    (as-> input $
      (map has-overlaps $)
      (.indexOf $ nil)
      (inc $))))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))

; OLD

(defn- solve1-old
  [input]
  (loop [[x & xs] input
         acc (transient {})]
    (if x (recur
           xs
           (assoc! acc x (inc (acc x 0))))
        (->> acc persistent!
             vals
             (remove (partial = 1))
             count))))

(defn- find-overlaps
  [points]
  (loop [[x & xs] points
         overlaps (transient #{})
         points (transient #{})]
    (if x (recur xs
                 (if (contains? points x)
                   (conj! overlaps x)
                   overlaps)
                 (conj! points x))
        (persistent! overlaps))))

(defn- solve1-over [input]
  (->> input find-overlaps count))