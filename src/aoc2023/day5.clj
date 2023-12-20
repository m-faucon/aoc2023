(ns aoc2023.day5
  (:require
    [aoc2023.util :as util]
    [clojure.string :as str]))

(defn parse-map-line
  [line]
  (->> (str/split line #" ")
       (map parse-long)
       (zipmap [:dst :src :ran])))

(defn parse-map
  [s]
  (->> (str/split-lines s)
       rest
       (mapv parse-map-line)
       ;; not necessary for part 1 but required for part 2
       (sort-by :src)))

(defn parse-input
  [input]
  (let [[seeds & maps] (str/split input #"\n\n")]
    {:seeds (->> (str/split seeds #" ")
                 rest
                 (mapv parse-long))
     :maps (mapv parse-map maps)}))

(defn find-in-map
  [lines i]
  (println i lines)
  ;; no need for binary search for such small vectors
  (some (fn [{:keys [src ran dst]}]
          (when (<= src i (+ src ran -1))
            (+ i dst (- src))))
        lines))

(defn step
  [i map-]
  (or (find-in-map map- i) i))

(defn find-in-maps
  [maps i]
  (reduce step i maps))

(let [{:keys [maps seeds]} (parse-input (util/slurp-input))]
  (apply min (map (partial find-in-maps maps) seeds)))

;; part 2

(defn interval-intersection
  [[a b] [c d]]
  [(max a c) (min b d)])

(defn empty-interval?
  [[m M]]
  (< M m))

(defn with-end
  [{:keys [src ran] :as line}]
  (assoc line :end (+ src ran -1)))

(defn add-unmapped-intervals
  [map-]
  (let [from-minus-infinity {:src Integer/MIN_VALUE
                             :end (-> map- first :src dec)
                             :dst :unmapped}
        last- (last map-)
        to-infinity {:src (-> last- :end inc)
                     :end Integer/MAX_VALUE
                     :dst :unmapped}]
    (->
     (into [from-minus-infinity]
           (mapcat (fn [[{:keys [end] :as f} {:keys [src]}]]
                     (let [middle-beg (inc end)
                           middle-end (dec src)]
                       (cond-> [f]
                         (not (empty-interval? [middle-beg middle-end]))
                         (conj {:src middle-beg :end middle-end :dst :unmapped})))))
           (partition 2 1 map-))
     (conj last-)
     (conj to-infinity))))

(defn interval-translate
  [[a b] t]
  [(+ a t) (+ b t)])

(defn process-interval
  [map- interval]
  (keep (fn [{:keys [src end dst]}]
          (let [inter (util/interval-intersection interval [src end])]
            (when-not (util/empty-interval? inter)
              (if (= :unmapped dst)
                inter
                (let [translation (- dst src)]
                  (interval-translate inter translation))))))
        map-))

(defn step2
  [intervals map-]
  (mapcat (partial process-interval map-)
          intervals))

(let [{:keys [maps seeds]} (parse-input (util/slurp-input))
      maps (map #(-> (map with-end %)
                     add-unmapped-intervals)
                maps)
      seed-intervals (->> (partition 2 seeds)
                          (map (fn [[beg ran]]
                                 [beg (+ beg ran -1)])))
      final-intervals (reduce step2
                              seed-intervals
                              maps)]
  (apply min (map first final-intervals)))
