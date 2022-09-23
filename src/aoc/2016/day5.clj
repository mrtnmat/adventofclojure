(ns aoc.2016.day5
  (:require [aoc.utils :as aoc])
  (:require [clj-commons.digest :as digest]))

; --- Day 5: How About a Nice Game of Chess? ---

(def default-input "ugkcyxxp")

(defn- hashes
  [door-id]
  (map #(digest/md5 (str door-id %)) (range)))

(defn- solve1
  [input]
  (->> (hashes input)
       (filter #(= (subs % 0 5) "00000"))
       (map #(get % 5))
       (take 8)
       (apply str)))

; --- Part 2 ---

(defn- solve2
  [input]
  (let [parsed-hashes (->> (hashes input)
                           (filter #(not= (re-find #"^00000[0-7]" %) nil))
                           (map #(hash-map (aoc/parse-int (str (get % 5)))
                                           (str (get % 6)))))]
    (loop [[x & xs] parsed-hashes
           acc {}]
      (if (= 8 (count acc))
        (->> acc
             (sort-by key <)
             (map second)
             (apply str))
        (recur xs (merge-with (fn [old _] old) acc x))))))