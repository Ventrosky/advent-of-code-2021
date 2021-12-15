#!/usr/bin/env bb

(def input (slurp "./input/day_15.txt"))

(def ^:private inf (Long/MAX_VALUE))

(defn update-costs
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
     (fn [c nbr nbr-cost]
       (if (unvisited nbr)
         (update-in c [nbr] min (+ curr-cost nbr-cost))
         c))
     costs
     (get g curr))))

(defn dijkstra
  ([g src dst]
   (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
          curr src
          unvisited (disj (apply hash-set (keys g)) src)]
     (cond
       (= curr dst) (get costs dst)
       (or (empty? unvisited) (= inf (get costs curr))) costs
       :else (let [next-costs (update-costs g costs unvisited curr)
                   next-node (apply min-key next-costs unvisited)]
               (recur next-costs next-node (disj unvisited next-node)))))))

(def size 100)

(defn in-bounds?
  [point]
  (let [check #(<= 0 %1 (dec size))]
    (every? check point)))

(defn neighbours
  [[x y] costs]
  (apply merge (map #(hash-map % (get costs %))
                    (filter in-bounds?
                            (for [p '([-1 0] [1 0] [0 1] [0 -1])]
                              (map + [x y] p))))))


(defn parse-tile
  [input]
  (let [costs (zipmap (for [i (range size) j (range size)] [i j])
                      (map #(Integer/parseInt %) (re-seq #"\d" input)))
        graph (reduce (fn [g n]
                        (assoc g n (neighbours n costs))) {} (keys costs))]
    graph));

(println "sol 1:" (dijkstra (parse-tile input) [0 0] [99 99]))