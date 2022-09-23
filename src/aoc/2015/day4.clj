(ns aoc.2015.day4
  (:require [aoc.utils :as aoc])
  (:require [clojure.string :as str])
  (:require [clj-commons.digest :as digest]))

; --- Day 4: The Ideal Stocking Stuffer ---

(def default-input "iwrupvqb")
(def test-input "abcdef")

(defn- starts-with-zeros [secret-key n]
  (->> (range)
       (map #(str secret-key %))
       (map #(vector % (digest/md5 %)))
       (remove #(some (partial not= \0) (subs (second %) 0 n)))))

(defn- solve1 [input]
  (-> input
      (starts-with-zeros 5)
      (first)
      (first)
      (str/replace input "")
      (aoc/parse-int)))

; --- Part Two ---

(defn- solve2 [input]
  (-> input
      (starts-with-zeros 6)
      (first)
      (first)
      (str/replace input "")
      (aoc/parse-int)))