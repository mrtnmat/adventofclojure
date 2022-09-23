(ns aoc.2015.day1)
 
(def input "src/aoc/2015/day1.txt")

(defn- parseC
  [c]
  (case c
    \( 1
    \) -1
    0))

(defn solve1
  "Returns the answer for part1"
  []
  (reduce #(+ %1 (parseC %2))
          0
          (seq (slurp input))))

(defn solve2
  "Returns the answer for part2"
  []
  (:step (first
          (drop-while #(not= (:floor %) -1)
                      (map #(hash-map :step %1 :floor %2)
                           (iterate inc 1)
                           (reductions + 0
                                       (map parseC (slurp input))))))))

(defn solve
  "Returns both answers."
  []
  (let [data (slurp input)
        floors (reductions
                + 0
                (map parseC data))
        path (map #(hash-map :step %1 :floor %2)
                  (iterate inc 1)
                  floors)
        from-basement (drop-while
                       #(not= (:floor %) -1)
                       path)]
    {:part1 (:floor (last path))
     :part2 (:step (first from-basement))}))