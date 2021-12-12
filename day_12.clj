#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (slurp "./input/day_12.txt"))

(def ^:const START "start")
(def ^:const END "end")

(defn parse-graph
  [input]
  (let [edges (mapv #(str/split % #"-") (str/split-lines input))
        nodes (re-seq #"\w+" input)
        empty-graph (reduce #(assoc %1 %2 []) {} nodes)]
    (reduce (fn [g [f t]]
              (update (update g f #(conj % t) ) t #(conj % f)))
            empty-graph
            edges)))

(defn is-small?
  [v]
  (re-matches #"^[a-z]+$" v))

(defn allowed-adj
  [path n s-limit]
  (let [npath (conj path n)
        small (filter is-small? npath)]
    (<= (count small) (+ (count (set small)) s-limit))))

(defn neighbors
  [v coll]
  (filter #(not= START %) (get coll v)))

(defn availables 
  [graph path pos limit]
  (filter #(allowed-adj path % limit) (neighbors pos graph)))

(defn find-paths
  [graph limit & [pos current]]
  (let [pos (or pos START)
        current (or current [])
        path (conj current pos)
        adj (availables graph path pos limit)]
    (if (= pos END) 
        path
        (mapcat #(find-paths graph limit % path) adj))))

(defn count-paths
  [input limit]
  (let [graph (parse-graph input)
        sum-segment #(+ (if (= %2 END) 1 0) %1)]
    (reduce sum-segment 0 (find-paths graph limit))))

(println "sol 1:" (count-paths input 0))

(println "sol 2:" (count-paths input 1))