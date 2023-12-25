(ns aoc2023.day24
  (:require
    [clojure.math.combinatorics :as combo]
    [aoc2023.util :as util]))

;; p0x + v0x * t0 = p1x + v1x * t1
;; p0y + v0y * t0 = p1y + v1y * t1
;;
;; <=>
;;
;; A*t0 + B*t1 = E
;; C*t0 + D*t1 = F
;;
;; with
;; A = v0x, B = -v1x, E = p1x - p0x
;; C = v0y, D = -v1y, F = p1y - p0y

(defn positive-time-intersection
  [[p0x p0y] [v0x v0y] [p1x p1y] [v1x v1y]]
  (let [A v0x B (- v1x) E (- p1x p0x)
        C v0y D (- v1y) F (- p1y p0y)
        det (- (* A D) (* B C))]
    (when-not (zero? det)
      (let [[t0 t1] [(/ (- (* E D) (* B F)) det)
                     (/ (- (* A F) (* E C)) det)]]
        (when (and (pos? t0) (pos? t1))
          [(+ p0x (* v0x t0))
           (+ p0y (* v0y t0))])))))

(defn inside-test-area?
  [[x y]]
  (when x
    (and (<= 200000000000000 x 400000000000000)
         (<= 200000000000000 y 400000000000000))))

(defn parse-line
  [line]
  (->> (re-find #"(-?\d+), +(-?\d+), +(-?\d+) @ +(-?\d+), +(-?\d+), +(-?\d+)" line)
       rest
       (map parse-long)
       (partition 3)))

(let [hailstones (map parse-line (util/input-lines))]
  (count (for [[[p0 v0] [p1 v1]] (combo/combinations hailstones 2)
               :let [xy (positive-time-intersection p0 v0 p1 v1)]
               :when (and (seq xy) (inside-test-area? xy))]
           xy)))


;; part 2
;;
;; later
