(ns aoc2023.day14
  (:require
   [aoc2023.util :as util]))

(defn apply-gravity
  [col]
  ; col means column not coll
  (->> (partition-by #{\#} col)
       (mapcat (fn [segment]
                 (if (= \# (first segment))
                   segment
                   (let [freqs (frequencies segment)]
                     (concat (repeat (freqs \O 0) \O)
                             (repeat (freqs \. 0) \.))))))))

(defn compute-load
  [lines]
  (let [H (count lines)]
    (->> lines
         (map-indexed (fn [i line]
                        (* (count (filter #{\O} line))
                           (- H i))))
         (reduce +))))

(->> (util/input-cols)
     (map apply-gravity)
     (apply mapv vector)
     compute-load)

;; part 2

(defn find-indexes-of-first-repetition
  [xs]
  (->> (map-indexed vector xs)
       (reduce (fn [acc [i x]]
                 (if-let [start (acc x)]
                   (reduced [start i])
                   (assoc acc x i)))
               {})))

(defn nth-of-eventually-periodic
  "nth for large n without realizing all the xs.
  Assumes xs are eventually periodic.
  Unlike nth, argument order is suited for ->>"
  [n xs]
  (let [[start end] (find-indexes-of-first-repetition xs)]
    (if (<= n start)
      (nth xs n)
      ; find m such that n ~ m (mod T) and start <= m < end
      ; (then we'll have x[n] = x[m]). equivalently,
      ; find m such that n ~ m (mod T) and 0 <= m - start < T
      ; in other words, calling p = m - start, find p such that
      ; n - start ~ p (mod T), and 0 <= p < T,
      ; that is, compute the remainder p = (n - start) mod T.
      (let [T (- end start)
            p (mod (- n start) T)
            m (+ p start)]
        (nth xs m)))))

(comment (nth-of-eventually-periodic 100000000000000000000 (cons 0 (cycle [1 2]))) )

(defn step
  [cols]
  ; hope the cycle isn't too far away because all this
  ; moving stuff around is extremely inefficient.
  (->> cols
       (map apply-gravity)
       (apply mapv vector) ; now we have lines
       (map apply-gravity)
       (apply mapv (comp reverse vector)) ; now reversed cols
       (map apply-gravity)
       (map reverse)
       (apply mapv (comp reverse vector)) ; now reversed lines
       (map apply-gravity)
       (map reverse)
       (apply mapv vector) ; back to cols
       ))

(->> (util/input-cols)
     (iterate step)
     (nth-of-eventually-periodic 1000000000)
     (apply mapv vector) ; compute-load takes in lines
     compute-load)
