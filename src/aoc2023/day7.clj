(ns aoc2023.day7
  (:require
    [aoc2023.util :as util]
    [clojure.string :as str]))


(defn freq-freq
  [xs]
  (frequencies (vals (frequencies xs))))

(defn card->type-rank
  [card]
  (let [ff (freq-freq card)]
    (cond
      (ff 5) 6 #_:five-of-a-kind
      (ff 4) 5 #_:four-of-a-kind
      (and (ff 3) (ff 2)) 4 #_:full-house
      (and (ff 3) (not (ff 2))) 3 #_:three-of-a-kind
      (= 2 (ff 2)) 2 #_:two-pair
      (= 1 (ff 2)) 1 #_:one-pair
      :else 0 #_:high-card)))

(def label->rank
  {\A 14
   \K 13
   \Q 12
   \J 11
   \T 10
   \9 9
   \8 8
   \7 7
   \6 6
   \5 5
   \4 4
   \3 3
   \2 2})


(defn card->ranker
  [card]
  (into [(card->type-rank card)]
        (map label->rank)
        card))

(defn parse-pre-process-line
  [line]
  (let [[card bid] (str/split line #" ")]
    {:ranker (card->ranker card)
     :bid (parse-long bid)}))

(->> (util/input-lines)
     (mapv parse-pre-process-line)
     (sort-by :ranker)
     (map-indexed (fn [idx {:keys [bid]}]
                    (* (inc idx) bid)))
     (reduce +))

;; part 2

;; didnt want to abstract stuff, just copy paste and add 2 to names

(def label->rank2
  (assoc label->rank \J 1))

(def all-non-J
  (keys (dissoc label->rank \J)))

(defn all-joker-substitions
  [[f & r]]
  (if f
    (let [rec (all-joker-substitions r)
          subs (if (= \J f)
                 all-non-J
                 [f])]
      (for [c subs
            rr rec]
        (cons c rr)))
    [[]]))

(defn card->type-rank-with-joker
  [card]
  (->> (all-joker-substitions card)
       (map card->type-rank)
       sort
       last))

(defn card->ranker2
  [card]
  (into [(card->type-rank-with-joker card)]
        (map label->rank2)
        card))

(defn parse-pre-process-line2
  [line]
  (let [[card bid] (str/split line #" ")]
    {:ranker (card->ranker2 card)
     :bid (parse-long bid)}))

(->> (util/input-lines)
     (mapv parse-pre-process-line2)
     (sort-by :ranker)
     (map-indexed (fn [idx {:keys [bid]}]
                    (* (inc idx) bid)))
     (reduce +))
