(ns aoc2023.day15
  (:require [clojure.string :as str]
            [aoc2023.util :as util]))

(defn aoc-hash
  [s]
  (reduce (fn [acc c] (-> acc (+ (int c)) (* 17) (mod 256))) 0 s))

(transduce
 (map aoc-hash)
 + (str/split (util/slurp-input) #","))

;; part 2

(defn process-segment
  [boxes segment]
  (let [[_ label [op] focal] (re-find #"([a-z]+)(-|=)(\d?)" segment)
        box-idx (aoc-hash label)
        focal (when focal (parse-long focal))]
    (assert op segment)
    (case op
      \-
      (update boxes box-idx (partial into [] (remove (comp #{label} first))))
      \=
      ;; could pull something like https://github.com/clj-commons/ordered
      ;; for maps that keep insertion order, but I'll just use vectors and do linear scan
      (update boxes
              box-idx
              (fn [box] (if-let [i (some (fn [j] (when (= label (first (box j))) j))
                                         (range (count box)))]
                          (assoc-in box [i 1] focal)
                          (conj box [label focal])))))))

(defn focusing-power
  [boxes]
  (transduce
   (comp (map-indexed
          (fn [i box]
            (map-indexed
             (fn [j [_ focal]]
               (* (inc i) (inc j) focal))
             box)))
         cat)
   + boxes))

(->> (str/split (util/slurp-input) #",")
     (reduce process-segment
             (vec (repeat 256 [])))
     focusing-power)
