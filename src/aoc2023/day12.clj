(ns aoc2023.day12
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2023.util :as util]))

;; part 1, straightforward and slow

(defn parse-line
  [line]
  (let [[_ row group-sizes] (re-find #"((?:\.|\?|#)+) (.*)" line)]
    {:row row
     :group-sizes (mapv parse-long (str/split group-sizes #","))}))

(defn row->group-sizes
  [row]
  (assert (every? #{\# \.} row))
  (->> (str/split row #"\.+")
       (remove empty?) ; if it begins with \.
       (mapv count)))

(defn question-mark-indexes
  [row]
  (keep-indexed (fn [i v] (when (= \? v) i)) row))

(defn apply-candidate-indexes
  [row indexes]
  (->>
   (reduce (fn [acc idx] (assoc acc idx \#))
           (into [] row)
           indexes)
   (replace {\? \.})
   (apply str)))

(defn valid-replacements?
  [group-sizes new-row]
  (= (row->group-sizes new-row) group-sizes))

(defn parsed-line->count
  [{:keys [row group-sizes]}]
  (let [qmi (question-mark-indexes row)
        all-candidate-indexes (combo/combinations qmi (- (reduce + group-sizes)
                                                         (count (filter #{\#} row))))]
    (->> all-candidate-indexes
         (map (partial apply-candidate-indexes row))
         (filter (partial valid-replacements? group-sizes))
         count)))

(transduce
 (comp (map parse-line)
       (map parsed-line->count))
 + (util/input-lines))

;; part 2

(defn haha-five-time-larger-now
  [{:keys [row group-sizes]}]
  {:row (str/join "?" (repeat 5 row))
   :group-sizes (into [] cat (repeat 5 group-sizes))})

(defn trim-dots
  [xs]
  (drop-while #{\.} xs))

(defn satisfy-group
  "Returns a seq of what's left after various ways of satisfying one group condition.
  Assumes xs are left-trimmed of any `.` and does the same to its output"
  [group-size [f & r :as xs]]
  (when f
    (case f
      \#
      (let [group-maybe (take group-size xs)
            [drop-this & whats-left] (drop group-size xs)]
        (when (and (= group-size (count group-maybe))
                   (every? #{\# \?} group-maybe)
                   (or (not drop-this)
                       (not= \# drop-this)))
          [(trim-dots whats-left)]))
      \?
      (concat (satisfy-group group-size (cons \# r)) (satisfy-group group-size (trim-dots r)))
      ;; else throw
      )))

(defn expand-counting
  ;; Given a fn of x -> xs, and a map of x -> cnt,
  ;; (such as returned by `frequencies`), returns a map of x -> cnt.
  ;; If you iterate several times, it is as if you had iterated mapcat
  ;; on the xs and then called frequencies, but much more efficient.
  [f m]
  (reduce (fn [m [x cnt]]
            (reduce (fn [m y]
                      (update m y #(+ cnt (or % 0))))
                    m
                    (f x)))
          {} m))

(defn step
  [{:keys [xs-cnt group-sizes]}]
  {:xs-cnt (expand-counting (partial satisfy-group (first group-sizes)) xs-cnt)
   :group-sizes (rest group-sizes)})

(defn init
  [xs group-sizes]
  {:xs-cnt {(drop-while #{\.} xs) 1} :group-sizes group-sizes})

(defn parsed-line->count-faster
  [{:keys [row group-sizes]}]
  (->> (init row group-sizes)
       (iterate step)
       (drop-while (comp seq :group-sizes))
       first
       :xs-cnt
       (keep (fn [[xs cnt]]
               (when-not (some #{\#} xs)
                 cnt)))
       (reduce +)))

(transduce
 (comp (map parse-line)
       (map haha-five-time-larger-now)
       (map parsed-line->count-faster))
 + (util/input-lines))
