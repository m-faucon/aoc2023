(ns aoc2023.day6
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;; time for high school math
;; b : press boost time
;; r : record time
;; T : race time
;; we want b * (T - b) > r
;; let's see when it's equal, that would be when
;; b^2 - Tb + r = 0
;; (then the admissible boost values are in between)
;; open high school math manual, we need to compute
;; delta = T^2 - 4r
;; and then the solutions are (T +- sqrt(delta)) / 2

(defn solve-for-equal-record
  [r T]
  (let [sq-delta (Math/sqrt (- (* T T) (* 4 r)))]
    [(/ (- T sq-delta) 2) (/ (+ T sq-delta) 2)]))

(defn number-of-ints-in-interval
  [[a b]]
  (inc (int (- (Math/floor b) (Math/ceil a)))))


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (->> (str/split line #"\s+")
                   rest
                   (map parse-long))))
       (apply mapv vector)))

(->> (parse-input (util/slurp-input))
     (map (fn [[T r]]
            (number-of-ints-in-interval (solve-for-equal-record r T))))
     (reduce *))

;; part 2

(defn parse-input2
  [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (->> (str/split line #"\s+")
                   rest
                   str/join
                   parse-long)))))

(let [[T r] (parse-input2 (util/slurp-input))]
  (number-of-ints-in-interval (solve-for-equal-record r T)))
