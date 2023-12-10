(ns aoc2023.day3
  (:require
    [aoc2023.util :as util]))

(defn tile-kind
  [c]
  (cond
    (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c) :digit
    (= \. c) :dot
    :else :symbol))

(defn line-with-coords
  [m y]
  (for [x (range (:L m))]
    {:xy [x y]
     :c (m [x y])}))

(defn y->numbers-seq
  [m y]
  (let [digit-tile? (comp #{:digit} tile-kind :c)]
    (->> (line-with-coords m y)
         (partition-by digit-tile?)
         (filter (comp digit-tile? first))
         (map (fn [tile-and-chars]
                {:number (parse-long (apply str (map :c tile-and-chars)))
                 :tiles (mapv :xy tile-and-chars)})))))

(defn numbers-seq
  "Returns a seq of {:number ..., tiles ...}"
  [m]
  (mapcat (partial y->numbers-seq m) (range (:H m))))

(defn has-symbol-neighbor?
  [m tiles]
  (some (comp #{:symbol} tile-kind m)
        (into #{} (mapcat (partial util/king-neighbors m)) tiles)))

(let [m (util/input-map)]
  (transduce (comp (filter (comp (partial has-symbol-neighbor? m) :tiles))
                   (map :number))
             + (numbers-seq m)))

;; part 2

(defn index-numbers
  [numbers-seq]
  (into {}
        (mapcat (fn [{:keys [number tiles]}]
                     (map (fn [tile] [tile {:number number :id (first tiles)}])
                          tiles)))
        numbers-seq))

(defn number-neighbors
  [m indexed-numbers tile]
  (->> (util/king-neighbors m tile)
       (keep indexed-numbers)
       (group-by :id)
       (map (comp :number first val))))

(defn gear-ratio
  [m indexed-numbers tile]
  (if (= \* (m tile))
    (let [num-neighbors (number-neighbors m indexed-numbers tile)]
      (if (= 2 (count num-neighbors))
        (* (first num-neighbors) (second num-neighbors))
        0))
    0))

(let [m (util/input-map)
      indexed-numbers (index-numbers (numbers-seq m))]
  (transduce (map (partial gear-ratio m indexed-numbers))
             + (util/tiles m)))
