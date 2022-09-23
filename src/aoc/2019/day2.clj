(ns aoc.2019.day2
  (:require [aoc.utils :as aoc]))

; --- Day 2: 1202 Program Alarm ---

(defn- opcode1 [mem ist]
  (let [adr1 (mem (+ ist 1))
        adr2 (mem (+ ist 2))
        out (mem (+ ist 3))]
    (assoc mem
           out
           (+ (mem adr1)
              (mem adr2)))))

(defn- opcode2 [mem ist]
  (let [adr1 (mem (+ ist 1))
        adr2 (mem (+ ist 2))
        out (mem (+ ist 3))]
    (assoc mem
           out
           (* (mem adr1)
              (mem adr2)))))

(def default-input [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 6 19 23 2 23 6 27 1 5 27 31 1 10 31 35 2 6 35 39 1 39 13 43 1 43 9 47 2 47 10 51 1 5 51 55 1 55 10 59 2 59 6 63 2 6 63 67 1 5 67 71 2 9 71 75 1 75 6 79 1 6 79 83 2 83 9 87 2 87 13 91 1 10 91 95 1 95 13 99 2 13 99 103 1 103 10 107 2 107 10 111 1 111 9 115 1 115 2 119 1 9 119 0 99 2 0 14 0])
(def test-input
  [[1 9 10 3 2 3 11 0 99 30 40 50] ; [3500 9 10 70 2 3 11 0 99 30 40 50]
   [1 0 0 0 99] ; [2 0 0 0 99]
   [2 3 0 3 99] ; [2 3 0 6 99]
   [2 4 4 5 99 0] ; [2 4 4 5 99 9801]
   [1 1 1 4 99 5 6 0 99] ; [30 1 1 4 2 5 6 0 99]
   ])

(defn- set-mem [mem [a b]]
  (assoc mem
         1 a
         2 b))

(defn- step [{mem :mem
              ist :ist}]
  (case (get mem ist)
    1 {:mem (opcode1 mem ist)
       :ist (+ 4 ist)}
    2 {:mem (opcode2 mem ist)
       :ist (+ 4 ist)}
    nil))

(defn- execute
  [{mem :mem
    ist :ist
    :as program}]
  (if (vector? program) (execute {:mem program :ist 0})
      (when mem
        (if (= (mem ist) 99)
          mem
          (recur (step program))))))

(defn- solve1
  [input]
  (first (execute {:mem input :ist 0})))

(def solver1 (aoc/solver {:f solve1
                          :default (set-mem default-input [12 2])
                          :parse nil}))

; --- Part Two ---

(defn- solve2
  ([] (solve2 default-input))
  ([input]
   (loop [[x & xs]
          (apply concat
                 (map #(map (partial vector %)
                            (range 0 100))
                      (range 0 100)))]
     (let [res (->> x
                    (set-mem input)
                    (execute)
                    (first))
           [a b] x]
       (if (= res 19690720)
         (->> a (* 100) (+ b))
         (recur xs))))))

(def solver2 (aoc/solver {:f solve2
                          :default default-input
                          :parse nil}))