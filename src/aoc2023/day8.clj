(ns aoc2023.day8
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

(defn parse-node
  [s]
  (->> (re-find #"(...) = \((...), (...)\)" s)
       rest
       (zipmap [:id \L \R])))

(defn parse-input
  [input]
  (let [[dirs nodes] (str/split input #"\n\n")]
    {:dirs dirs
     :network (->> (str/split-lines nodes)
                   (map parse-node)
                   (into {} (map (juxt :id #(dissoc % :id)))))}))

(defn id-update
  [network dir id]
  (-> network (get id) (get dir)))

(defn step-fn
  [network]
  (fn [[cnt id] dir]
    (if (= "ZZZ" id)
      (reduced cnt)
      [(inc cnt) (id-update network dir id)])))

(let [{:keys [dirs network]} (parse-input (util/slurp-input))]
  (reduce (step-fn network)
          [0 "AAA"]
          (cycle dirs)))

;; part 2

;; doing it dumbly takes too much time,
;; but since the cycles are independant,
;; the length of the product cycle
;; is the least commun multiple of the lengths

(def id-end-in-z?
  (partial re-find #"..Z"))

(def id-end-in-a?
  (partial re-find #"..A"))

(defn step-fn2
  [network]
  (fn [[cnt id] dir]
    (if (id-end-in-z? id)
      (reduced cnt)
      [(inc cnt) (id-update network dir id)])))

(defn go
  [start-id]
  (let [{:keys [dirs network]} (parse-input (util/slurp-input))]
    (reduce (step-fn2 network)
            [0 start-id]
            (cycle dirs))))

(let [{:keys [network]} (parse-input (util/slurp-input))]
  (map go (filter id-end-in-a? (keys network))))

;; => (20513 18827 17141 22199 13207 12083)

;; paste that in an online lcm calculator
;;
;; => 13385272668829
