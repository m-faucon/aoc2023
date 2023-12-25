(ns aoc2023.day25
  (:require
   [aoc2023.util :as util]
   [clojure.set :as set]
   [clojure.string :as str]))

;; https://en.wikipedia.org/wiki/Karger's_algorithm

(defn parse-input
  [input]
  (reduce (fn [graph line]
            (let [[_ in outs] (re-find #"(...): (.*)" line)
                  outs (str/split outs #" ")]
              (reduce (fn [graph out]
                        (-> graph
                            (update in (fnil conj #{}) out)
                            (update out (fnil conj #{}) in)))
                      graph
                      outs)))
          {}
          (str/split-lines input)))

(defn rename-outs
  [graph old-name new-name]
  (reduce (fn [graph out]
            (update graph out #(-> % (disj old-name) (conj new-name))))
          graph
          (graph old-name)))

(defn contract-nodes
  [graph n m]
  (let [new-node (str n "-" m)]
    (-> graph
        (rename-outs n new-node)
        (rename-outs m new-node)
        (dissoc n m)
        (assoc new-node (disj (set/union (graph n) (graph m)) n m)))))

(defn contract-random-edge
  [graph]
  (let [n (rand-nth (keys graph))
        m (rand-nth (seq (graph n)))]
    (contract-nodes graph n m)))

(defn edge-cuts
  [graph cc]
  (for [node cc
        out (graph node)
        :when (not (cc out))]
    [node out]))

(defn random-cut
  [graph]
  (->> (iterate contract-random-edge graph)
       (drop-while #(< 2 (count %)))
       first))

(defn three-cut?
  [graph contracted-graph]
  (let [cc (-> contracted-graph first key (str/split #"-") set)]
    (= 3 (count (edge-cuts graph cc)))))

(defn cut-score
  [contracted-graph]
  (let [cc1 (str/split (key (first contracted-graph)) #"-")
        cc2 (str/split (first (val (first contracted-graph))) #"-")]
    (* (count cc1) (count cc2))))

(let [graph (parse-input (util/slurp-input))]
  (->> (repeatedly (partial random-cut graph))
       (filter (partial three-cut? graph))
       first
       cut-score))

;; part 2
;;
;; need to solve the rest first
