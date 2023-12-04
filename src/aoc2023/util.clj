(ns aoc2023.util
  (:require
   [clojure.java.shell :as shell]
   [clojure.string :as str]))

(defn day
  []
  (->> (str/split (str *ns*) #"\.")
       last
       (re-find #"\d+")
       parse-long))

(defn slurp-input
  ([] (slurp-input 1))
  ([i] (str/trimr (slurp (str "resources/day" (day) "-" i)))))

(defn input-lines
  ([] (input-lines 1))
  ([i]
   (->> (slurp (str "resources/day" (day) "-" i))
        str/split-lines)))

(defn input-map
  ([] (input-map 1))
  ([i]
   (let [lines (vec (input-lines i))
         H (count lines)
         L (count (lines 0))]
     (into {:H H :L L}
           (for [x (range L)
                 y (range H)]
             [[x y] (.charAt (lines y) x)])))))

(defn tiles
  [m]
  (remove #{:H :L} (keys m)))

(defn king-neighbors
  "9 diagonals neighbors.
  Pass {:keys [L H]} as first argument to remove illegal tiles."
  ([[x y]]
   #{[(inc x) (dec y)]
     [(inc x) y]
     [(inc x) (inc y)]
     [x (dec y)]
     [x (inc y)]
     [(dec x) (dec y)]
     [(dec x) y]
     [(dec x) (inc y)]})
  ([{:keys [L H]} tile]
   (into #{} (filter #(and (<= 0 (% 0) (dec L))
                           (<= 0 (% 1) (dec H)))
                     (king-neighbors tile)))))

;; didn't use that in the end
(defn all-matches
  [re s]
  (let [matcher (re-matcher re s)]
    (take-while some? (repeatedly #(re-find matcher)))))
