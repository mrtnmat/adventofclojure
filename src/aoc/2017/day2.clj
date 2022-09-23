(ns aoc.2017.day2
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

(def default-input "src/aoc/2017/day2.txt")
(def test-input "src/aoc/2017/day2test.txt")

(defn- parse-line
  [line]
  (map #(Integer/parseInt %) (str/split line #"[\t\s]")))

(defn- parse
  [filename]
  (map #((comp vec parse-line) %) (aoc/parse-lines filename)))

(def stress-input (->> default-input parse cycle))

; part1

(defn- max-min
  [row]
  (let [sorted (sort row)]
    (- (last sorted) (first sorted))))

(defn- solver1
  [input]
  (apply + (map max-min input)))

; part2

(defn- divisible
  ([[x y]] (divisible x y))
  ([x y]
   (zero? (mod x y))))

(defn- ffff
  [[x & xs]]
  (when-let [tail (seq xs)]
     (cons (->> tail
                (map (partial / x))
                (filter int?)
                (first))
           (ffff tail))))

(defn- solver2
  [input]
  (->> input
       (map
        #(->> %
              (sort >)
              ffff
              (drop-while nil?)
              first))
       (apply +)))

(defn- find-divisible
  [row]
  (loop [[x & xs] (sort > row)]
    (when xs
      (let
       [divisor (->> xs
                     (drop-while
                      (complement (partial divisible x)))
                     (first))]
        (if divisor
          (/ x divisor)
          (recur xs))))))

(defn- solver2-old
  [input]
  (->> input (map find-divisible) (apply +)))

; Paolo's approach
(defn- f [coll]
  (when-let [[x & xs] (seq coll)]
    (concat
     (map (partial list x) xs)
     (f xs))))

(defn- solver2-rec
  [input]
  (->> input
       (map (comp
             (fn [[x y]] (/ x y))
             first
             (partial filter divisible)
             f
             (partial sort >)))
       (apply +)))

(def solve1 (aoc/solver {:f solver1
                         :default default-input
                         :parse parse}))

(def solve2 (aoc/solver {:f solver2
                         :default default-input
                         :parse parse}))