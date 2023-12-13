(ns aoc2023.day13
  (:require
    [aoc2023.util :as util]
    [clojure.string :as str]))

(defn is-reflection-center?
  [v x]
  (every? (fn [dx] (= (v (- x dx)) (v (+ x dx 1))))
          (range (min (inc x) (- (count v) x 1)))))

(defn find-reflection-center
  [v]
  (some (fn [x] (when (is-reflection-center? v x) x))
        (range (dec (count v)))))

(defn paragraph-summary
  [p]
  (let [lines (str/split-lines p)]
    (if-let [y (find-reflection-center (vec lines))]
      (* 100 (inc y))
      (if-let [y (find-reflection-center (apply mapv vector lines))]
        (inc y)
        (throw (ex-info "couldn't find reflection" {:lines lines}))))))

(transduce
 (map paragraph-summary)
 + (util/input-paragraphs))

;; part 2

(defn number-of-equal-elts
  [v w]
  (assert (= (count v) (count w)))
  (count (filter identity (map = v w))))

(defn is-almost-reflection-center?
  [v x]
  (let [size (count (v 0))
        n (min (inc x) (- (count v) x 1))
        freqs (frequencies
               (map (fn [dx] (number-of-equal-elts (v (- x dx)) (v (+ x dx 1)))) 
                    (range n)))] 
    (or (= freqs {size (dec n) (dec size) 1})
        (= freqs {(dec size) 1}))))

;; again, why abstract when I can just copy paste and never touch it again
 
(defn find-almost-reflection-center
  [v]
  (some (fn [x] (when (is-almost-reflection-center? v x) x))
        (range (dec (count v)))))

(defn paragraph-summary2
  [p]
  (let [lines (str/split-lines p)]
    (if-let [y (find-almost-reflection-center (vec lines))]
      (* 100 (inc y))
      (if-let [y (find-almost-reflection-center (apply mapv vector lines))]
        (inc y)
        (throw (ex-info "couldn't find almost reflection" {:lines lines}))))))

(transduce
 (map paragraph-summary2)
 + (util/input-paragraphs))
