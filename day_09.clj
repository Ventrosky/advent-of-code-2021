#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_09.txt")))

;; ------------------------------------ part 1

(defn str->int
  [s]
  (Integer/parseInt s))

(defn parse-row
  [s]
  (map str->int (str/split s #"")))

(def WIDTH (count (parse-row (first input))))
(def HEIGHT (count input))

(defn make-nodes-map
  [input]
  (let [vertices (for [j (range HEIGHT) i (range WIDTH)] [i j])
        weights (flatten (map parse-row input))]
    (zipmap vertices weights)))

(defn adjacent-vertices
  [vertex]
  (let [in-bounds? #(<= 0 %1 (dec %2))
        neighbors '([-1 0] [1 0] [0 1] [0 -1])]
    (filter identity (map #(let [[x2 y2] (map + vertex %)]
                              (when (and (in-bounds? x2 WIDTH) (in-bounds? y2 HEIGHT)) [x2 y2]))
                          neighbors))))

(defn augment-vertex
  [nodes-map pair]
  (let [[vertex w] pair
        x (apply min (map #(nodes-map %) (adjacent-vertices vertex)))]
     {:point vertex :risk (inc w) :low-point? (< w x)}))

(defn find-low-points
  [nodes-map]
  (filter :low-point? (map (partial augment-vertex nodes-map) nodes-map)))

(defn low-points-sum 
  [input]
  (let [nodes-map (make-nodes-map input)]
   (reduce #(+ %1 (:risk %2)) 0 (find-low-points nodes-map))))

(println "sol 1:" (low-points-sum input))

;; ------------------------------------ part 2

(def WALL 9)

(defn basin-size [nodes-map low-point]
  (loop [locations #{}
         explored #{}
         vertices [(:point low-point)]]
    (if (empty? vertices)
      (count locations)
      (let [vertex (first vertices)
            remains (rest vertices)
            w (nodes-map vertex)]
        (if (or (explored vertex) (= w WALL))
          (recur locations explored remains)
          (recur (conj locations vertex)
                 (conj explored vertex)
                 (into remains (adjacent-vertices vertex))))))))

(defn basins-sizes
  [nodes-map]
  (map #(basin-size nodes-map %) (find-low-points nodes-map)))

(defn largest-basins 
  [input]
  (let [nodes-map (make-nodes-map input)]
    (apply * (take-last 3 (sort (basins-sizes nodes-map))))))

(println "sol 2:" (largest-basins input))