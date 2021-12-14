#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_14.txt")))

(defn parse-input 
  [input]
  (let [initial (str/split (first input) #"")
        pairs (map #(re-matches #"(\w)(\w) -> (\w)" %) (drop 2 input))
        rules (reduce (fn [dicto [_ x y z]] (assoc dicto [x y] z)) {} pairs)
        dicto (zipmap (map vec (partition 2 1 initial)) (repeat 1))]
    {:dicto dicto :rules rules :freq (frequencies initial)}))

(defn expand-key
  [rules [k v]]
  (let [n (get rules k)
        [c1 c2] k]
    {:new-dicto {[c1 n] v [n c2] v} :new-freq {n v}}))

(defn split-solutions 
  [rules [a1 b1] e] 
  (let [{:keys [new-dicto new-freq]} (expand-key rules e)]
    [(conj a1 new-dicto) (conj b1 new-freq)]))

(defn next-step
  [data]
  (let [{:keys [dicto rules freq]} data
        compute-new (partial split-solutions rules)
        [new-d new-f] (reduce compute-new [[] []] (seq dicto))]
    (assoc data 
           :dicto (apply merge-with + new-d)
           :freq (apply merge-with + (conj new-f freq)))))

(defn generator
  [input]
  (let [state (parse-input input)]
    (iterate next-step state)))

(defn polymerization
  [input steps]
  (let [data (nth  (generator input) steps) 
        s-freq (sort-by val (:freq data))]
    (- (second (last s-freq)) (second (first s-freq)))))

(println "sol 1:" (polymerization input 10))

(println "sol 2:" (polymerization input 40))