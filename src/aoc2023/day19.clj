(ns aoc2023.day19
  (:require
   [aoc2023.util :as util]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn parse-workflow
  [s]
  (let [[_ k content] (re-find #"([a-z]+)\{(.*)\}" s)]
    [(symbol k)
     (mapv (fn [rule]
             (let [split (str/split rule #":")]
               (if (= 2 (count split))
                 (let [[_ rating-key op num] (re-find #"([a-z]+)(<|>)(\d+)" (first split))
                       rating-key (symbol rating-key)
                       op (if (= "<" op) < >)
                       num (parse-long num)
                       target (symbol (second split))]
                   (fn [rating] (when (op (rating rating-key) num) target)))
                 (constantly (symbol (first split))))))
           (str/split content #","))]))

(defn accepted-rating?
  [workflows rating]
  (loop [workflow-id 'in]
    (cond
      (= 'A workflow-id) true
      (= 'R workflow-id) false
      :else (recur (some #(% rating) (workflows workflow-id))))))

(defn parse-rating
  [s]
  (edn/read-string (str/replace s "=" " ")))

(defn parse-input
  [input]
  (let [[ws rs] (str/split input #"\n\n")]
    {:workflows (into {} (map parse-workflow) (str/split-lines ws))
     :ratings (map parse-rating (str/split-lines rs))}))

(let [{:keys [ratings workflows]} (parse-input (util/slurp-input))]
  (transduce
   (comp (filter (partial accepted-rating? workflows))
         (mapcat vals))
   + ratings))

;; part 2

;; it turns out data is better than opaque functions
;; who could have predicted it
;; now we have to change this

(defn parse-workflow-as-data-this-time
  [s]
  (let [[_ k content] (re-find #"([a-z]+)\{(.*)\}" s)]
    [(keyword k)
     (mapv (fn [rule]
             (let [split (str/split rule #":")]
               (if (= 2 (count split))
                 (let [[_ rating-key op num] (re-find #"([a-z]+)(<|>)(\d+)" (first split))]
                   {:rating-key (keyword rating-key)
                    :num (parse-long num)
                    :op op
                    :target (keyword (second split))})
                 {:target (keyword (first split))})))
           (str/split content #","))]))

(defn update-intervals
  [intervals rating-key interval]
  (let [new-interval (util/interval-intersection (intervals rating-key) interval)]
    (when-not (util/empty-interval? new-interval)
      (assoc intervals rating-key new-interval))))

(defn split-rule
  [{:keys [num op]}]
  (case op
    "<" [[1 (dec num)] [num 4000]]
    ">" [[(inc num) 4000] [1 num]]))

(defn children-fn
  [workflows]
  (fn [{:keys [intervals next-workflow-id]}]
    (loop [[{:keys [rating-key op target] :as rule} & rules] (workflows next-workflow-id)
           intervals intervals
           res []]
      (assert rule)
      (if-not op
        (conj res {:intervals intervals :next-workflow-id target})
        (let [[out-interval continue-interval] (split-rule rule)
              out-intervals (update-intervals intervals rating-key out-interval)
              continue-intervals (update-intervals intervals rating-key continue-interval)]
          ;; probably there's a better way to say this
          (case [(boolean out-intervals) (boolean continue-intervals)]
            [true true] (recur rules
                               continue-intervals
                               (conj res {:intervals out-intervals :next-workflow-id target}))
            [false true] (recur rules continue-intervals res)
            [true false] (conj res {:intervals out-intervals :next-workflow-id target})
            #_else-throw))))))

(defn size
  [elt]
  (->> (:intervals elt)
       vals
       (map (fn [[a b]] (inc (- b a))))
       (reduce *)))

(let [workflows (->> (util/input-paragraphs) first
                     (str/split-lines)
                     (into {} (map parse-workflow-as-data-this-time)))]
  (->> {:intervals (zipmap [:x :m :a :s] (repeat [1 4000])) :next-workflow-id :in}
       (tree-seq (comp workflows :next-workflow-id)
                 (children-fn workflows))
       (filter (comp #{:A} :next-workflow-id))
       (map size)
       (reduce +)))
