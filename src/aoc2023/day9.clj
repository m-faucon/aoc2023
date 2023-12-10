(ns aoc2023.day9
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

(defn parse-line
  [line]
  (mapv parse-long
        (str/split line #" ")))

(defn diff
  [xs]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 xs)))

(defn diffs
  [xs]
  (->> (iterate diff xs)
       (take-while (partial some (complement zero?)))))

(defn find-next
  [xs]
  (->> (diffs xs)
       (map peek)
       (reduce +)))

(transduce
 (comp (map parse-line)
       (map find-next))
 + (util/input-lines))

;; part 2

(defn find-previous
  [xs]
  (->> (diffs xs)
       (map first)
       reverse
       (reduce (fn [acc v]
                 (- v acc))
               0)))
(transduce
 (comp (map parse-line)
       (map find-previous))
 + (util/input-lines))
