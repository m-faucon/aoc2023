(ns aoc2023.day4
  (:require
   [aoc2023.util :as util]
   [clojure.string :as str]))

(defn parse-line
  [line]
  (->> (re-find #"Card\s+\d+:\s+([^|]+)\s+\|\s+(.*)" line)
       rest
       (map (fn [s]
              (mapv parse-long (str/split s #" +"))))
       (zipmap [:winning-numbers :numbers-you-have])))

(defn card-score
  [{:keys [winning-numbers numbers-you-have]}]
  (let [n (count (filter (set winning-numbers) numbers-you-have))]
    (if (zero? n) 0 (int (Math/pow 2 (dec n))))))

(transduce
 (comp (map parse-line) (map card-score)) 
 + (util/input-lines))

;; part 2

(defn card-num-matches
  [{:keys [winning-numbers numbers-you-have]}]
  (count (filter (set winning-numbers) numbers-you-have)))

(defn parse-index-cards
  [lines]
  (into [nil] ; starts at index 1
        (comp (map parse-line)
              (map #(assoc % :count 1)))
        lines))

(defn update-counts
  [cards i n mult]
  (reduce (fn [cards j]
            (update-in cards [j :count] (partial + mult)))
          cards
          (range (inc i) (+ i n 1))))

(loop [cards (parse-index-cards (util/input-lines))
       i 1]
  (if (= i (count cards))
    (transduce (comp (drop 1) (map :count))
               + cards)
    (recur (let [card (cards i)
                 score (card-num-matches card)
                 mult (:count card)]
             (update-counts cards i score mult))
           (inc i))))
