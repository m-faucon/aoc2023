(ns aoc2023.day11
  (:require
   [clojure.math.combinatorics :as combo]
   [aoc2023.util :as util]))

(defn double-empty-rows
  [rows]
  (mapcat (fn [row] (if (every? #{\.} row) [row row] [row])) rows))

(defn expand-universe
  [rows]
  (->> (double-empty-rows rows)
       (apply mapv vector)
       double-empty-rows
       (apply mapv vector)))

(defn universe->galaxies
  [rows]
  (->> rows
       (map-indexed
        (fn [y row]
          (keep-indexed
           (fn [x c] (when (#{\#} c) [x y]))
           row)))
       (into [] cat)))

(defn galaxies->distances
  [galaxies]
  (map (partial apply util/manhattan-distance)
       (combo/combinations galaxies 2)))

(->> (util/input-lines)
     expand-universe
     universe->galaxies
     galaxies->distances
     (reduce +))

;; part 2, ofc this works also for part 1

(defn distance-accounting-for-expansion-1d
  [expansion a b]
  (let [m (min a b)
        M (max a b)]
    (reduce + (subvec expansion m M))))

(defn distance-accounting-for-expansion
  [expansion-x expansion-y [a0 a1] [b0 b1]]
  (+ (distance-accounting-for-expansion-1d expansion-x a0 b0)
     (distance-accounting-for-expansion-1d expansion-y a1 b1)))

(defn rows->expansion-vector
  [multiplier rows]
  (mapv (fn [row] (if (every? #{\.} row) multiplier 1)) rows))

(let [rows (util/input-lines)
      expansion-y (rows->expansion-vector 1000000 rows)
      expansion-x (rows->expansion-vector 1000000 (apply mapv vector rows))
      galaxy-pairs (combo/combinations (universe->galaxies rows) 2)
      distance (partial distance-accounting-for-expansion expansion-x expansion-y)]
  (transduce
   (map (partial apply distance))
   + galaxy-pairs))
