(ns aoc.2016.day4
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str]))

; --- Day 4: Security Through Obscurity ---

(def default-input "src/aoc/2016/day4.txt")
(def test-input "src/aoc/2016/day4-test.txt")

(defn- parse-line
  "aaaaa-bbb-z-y-x-123[abxyz] -> [aaaaabbbzyx 123 abxyz]"
  [s]
  (-> s
      (str/replace #"\-(\d+)" " $1")
      (str/replace #"[\-\]]+" "")
      (str/split #"[\[ ]")
      ))

(defn- my-comparator [[c1 n1] [c2 n2]]
  (let [diff1 (compare n2 n1)]
    (if (= 0 diff1)
      (compare c1 c2)
      diff1)))

(defn- parse
  [filename]
  (->> filename
       (aoc/slurp-lines)
       (map parse-line)))

(defn- checksum
  [s]
  (->> (str/replace s #" " "") frequencies (sort my-comparator) (map first) (take 5) (apply str)))

(defn- solve1
  [input]
  (->> input
       (filter #(= (checksum (% 0)) (% 2)))
       (map #(get % 1))
       (map aoc/parse-int)
       (apply +)))

; --- Part Two ---

(def alphabet [\a  \b  \c  \d  \e  \f  \g
               \h  \i  \j  \k  \l  \m  \n
               \o  \p  \q  \r  \s  \t  \u
               \v  \w  \x  \y  \z])

(defn- rotate-letter
  [n letter]
  (if (= letter \space) letter
      (as-> letter $
        (.indexOf alphabet $)
        (+ n $)
        (mod $ (count alphabet))
        (alphabet $))))

(defn- rotate-word
  [n word]
  (apply str (map #(rotate-letter n %) word)))

(defn- parse-line2
  "aaaaa-bbb-z-y-x-123[abxyz] -> [(aaaaa bbb z y x) 123]"
  [s]
  (-> s
      (str/replace #"([a-z]+)-" "$1 ")
      (str/replace #"([a-z]+) ([\d]+)\[([a-z]+)\]" "$1-$2-$3")
      (str/split #"-")))

(defn- parse2
  [filename]
  (->> filename
       (aoc/slurp-lines)
       (map parse-line2)))

(defn- solve2
  [input]
  (->> input
       (filter #(= (checksum (% 0)) (% 2)))
       (map (fn [[s n]] (vector s (aoc/parse-int n))))
       (map (fn [[s n]]
              (vector (rotate-word n s)
                      n)))
       ))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse2}))