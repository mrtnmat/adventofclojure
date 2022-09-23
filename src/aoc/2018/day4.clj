(ns aoc.2018.day4
  (:require [aoc.utils :as aoc]))

; --- Day 4: Repose Record ---

(def default-input "src/aoc/2018/day4.txt")
(def test-input "src/aoc/2018/day4-test.txt")

(defn- parse-line
  "[1518-11-01 00:05] falls asleep" ; [5 :sleep]
  [s]
  (let [data (re-find #":(\d\d)] (.*)" s)
        minutes (aoc/parse-int (data 1))]
    (case (first (data 2))
      \G (aoc/parse-int (re-find #"\d+" (data 2)))
      \w [minutes :wake]
      \f [minutes :sleep])))

(defn- minutes-asleep
  "([5 :sleep] [25 :wake] [30 :sleep] [55 :wake])"
  [events]
  (flatten (map #(range (first %1) (first %2))
                (take-nth 2 events)
                (take-nth 2 (drop 1 events)))))

(defn- split-by-guard
  [data]
  (loop [[guard-number & xs] data
         acc {}]
    (if (seq xs)
      (let [events (take-while vector? xs)
            tail (drop-while vector? xs)]
        (recur tail
               (update
                acc
                guard-number
                #(concat % (minutes-asleep events)))))
      (remove #(empty? (second %1)) acc))))

(defn- parse
  [filename]
  (->> filename aoc/slurp-lines sort (map parse-line)))

(defn- compute-total-sleep
  [data]
  (zipmap
   (keys data)
   (map #(apply + %) (vals data))))

(defn- solve1
  [input]
  (let [data (->> input split-by-guard)
        sleepiest (->> data compute-total-sleep (apply max-key val) key)
        sleepiest-minute (->> (data sleepiest) frequencies (apply max-key val) key)]
    (* sleepiest sleepiest-minute)))

; --- Part Two ---

(defn- solve2
  [input]
  (let [data (->> input split-by-guard)
        sleep-freq (zipmap (keys data)
                           (->> data vals (map #(->> % frequencies (apply max-key val)))))
        sleepiest (->> sleep-freq (apply max-key #(second (val %))))]
    (* (first sleepiest) (first (second sleepiest)))))

; --- Solvers ---

(def solver1 (aoc/solver {:f solve1
                          :default default-input
                          :parse parse}))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse parse}))