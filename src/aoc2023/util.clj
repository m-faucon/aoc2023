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

(defn input-cols
  ([] (input-cols 1))
  ([i]
   (->> (input-lines i)
        (apply mapv vector))))


(defn input-paragraphs
  ([] (input-paragraphs 1))
  ([i]
   (str/split (slurp-input i) #"\n\n")))

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
  [{:keys [L H]}]
  (for [x (range L) y (range H)] [x y]))

(defn update-tiles-vals
  [m f]
  (reduce (fn [m tile] (update m tile f)) m (tiles m)))

(defn tile-diff
  [[x0 y0] [x1 y1]]
  [(- x1 x0) (- y1 y0)])

(defn tile-add
  [[x0 y0] [x1 y1]]
  [(+ x1 x0) (+ y1 y0)])

(defn tile-scale
  [a [x y]]
  [(* a x) (* a y)])

(def up [0 -1])
(def down [0 1])
(def right [1 0])
(def left [-1 0])

(defn inside-tile?
  [{:keys [L H]} [x y]]
  (and (<= 0 x (dec L))
       (<= 0 y (dec H))))

(defn manhattan-neighbors
  "4 manhattan neighbors
  Pass {:keys [L H]} as first argument to remove illegal tiles."
  ([[x y]]
   [[(inc x) y]
    [(dec x) y]
    [x (inc y)]
    [x (dec y)]])
  ([m tile]
   (filterv (partial inside-tile? m)
            (manhattan-neighbors tile))))

(defn king-neighbors
  "9 diagonals neighbors.

  Pass {:keys [L H]} as first argument to remove illegal tiles."
  ([[x y]]
   [[(inc x) (dec y)]
    [(inc x) y]
    [(inc x) (inc y)]
    [x (dec y)]
    [x (inc y)]
    [(dec x) (dec y)]
    [(dec x) y]
    [(dec x) (inc y)]])
  ([m tile]
   (filterv (partial inside-tile? m)
            (king-neighbors tile))))

(defn manhattan-distance
  [[a0 a1] [b0 b1]]
  (+ (Math/abs (- b1 a1)) (Math/abs (- b0 a0))))

(defn parse-digit
  [c]
  #_(int \0)
  (- (int c) 48))

(defn interval-intersection
  [[a b] [c d]]
  [(max a c) (min b d)])

(defn empty-interval?
  [[m M]]
  (< M m))
