(ns aoc2023.day23
  (:require
   [aoc2023.util :as util]))

(defn next-tiles
  [carte position]
  (let [next-tiles
        (case (carte position)
          \. (util/manhattan-neighbors carte position)
          \> [(util/tile-add position util/right)]
          \< [(util/tile-add position util/left)]
          \v [(util/tile-add position util/down)]
          \^ [(util/tile-add position util/up)])]
    (remove (comp #{\# :visited} carte) next-tiles)))

(defn children
  [{:keys [carte tile] :as state}]
  (loop [tile tile
         carte carte
         cnt 0]
    (if (= tile [(-> carte :L dec dec) (-> carte :H dec)])
      [(-> (assoc state
                  :done true?)
           (assoc-in [:carte tile] :visited)
           (update :cnt (partial + cnt)))]
      (let [tiles (next-tiles carte tile)]
        (case (count tiles)
          0 nil
          1 (recur (first tiles) (assoc carte tile :visited) (inc cnt))
          #_else  (map (fn [new-tile] (-> state
                                          (assoc-in [:carte tile] :visited)
                                          (update :cnt (partial + cnt 1))
                                          (assoc :tile new-tile)))
                       tiles))))))

(->> {:carte (util/input-map) :tile [1 0] :cnt 0}
     (tree-seq (constantly true)
               children)
     (filter :done)
     (map :cnt)
     (apply max))

;; part 2
;;
;; let's be a bit more efficient and compact linear paths

(defn intersection-tiles
  [{:keys [L H] :as carte}]
  (for [x (range L)
        y (range H)
        :let [tile [x y]]
        :when (and (= \. (carte tile))
                   (< 2 (count (filter (comp #{\.} carte) (util/manhattan-neighbors tile)))))]
    tile))

(defn intersection->edges
  [carte intersection]
  (->> (util/manhattan-neighbors intersection)
       (filter (comp #{\.} carte))
       (map (fn [tile]
              (loop [tile tile
                     previous-tile intersection
                     cnt 0]
                (let [neighbors (->> (util/manhattan-neighbors tile)
                                     (filter (comp #{\.} carte))
                                     (remove #{previous-tile}))]
                  (if (= 1 (count neighbors))
                    (recur (first neighbors) tile (inc cnt))
                    [tile cnt])))))
       (into {})))

(defn carte->end-tile
  [{:keys [L H]}]
  [(dec (dec L)) (dec H)])

(defn all-paths
  [graph]
  (tree-seq (constantly true)
            (fn [{:keys [seen node] :as state}]
              (keep (fn [[next-node length]]
                      (when-not (seen next-node)
                        (-> (update state :total + length 1)
                            (update :seen conj next-node)
                            (assoc :node next-node)
                            (update :debug-path conj next-node))))
                    (graph node)))
            {:seen #{} :node [1 0] :total 0 :debug-path []}))

;; still takes a coupe minutes

(let [carte (util/update-tiles-vals (util/input-map) {\. \. \# \# \< \. \> \. \v \. \^ \.})
      end-tile (carte->end-tile carte)
      start-tile [1 0]]
  (->> (intersection-tiles carte)
       (cons start-tile)
       (map (juxt identity (partial intersection->edges carte)))
       (into {})
       all-paths
       (keep (fn [{:keys [node total]}]
               (when (= end-tile node) total)))))

(comment
  (defn viz
    [{:keys [H L] :as carte} debug-path]
    (let [debug-path (into {} (map-indexed (fn [idx tile] [tile idx]) debug-path))]
      (println "\n\n-------------------------------------------------")
      (run! (fn [y]
              (println (apply str (for [x (range L) :let [tile [x y]]]
                                    (or (debug-path tile) (carte tile))))))
            (range H)))))
