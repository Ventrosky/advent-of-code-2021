#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_05.txt")))

(defn str->int
  [s]
  (Integer/parseInt s))

(def pattern  #"^(\d+),(\d+)[^\d]+(\d+),(\d+)$")

(defn point->seq
  [z1 z2]
  (let [step (if (> z1 z2) -1 1)]
   (if (= z1 z2)
    (repeat z1)
    (range z1 (+ z2 step) step))))

(defn parse-row
  [s]
  (let [[_ & coords] (re-matches pattern s)
        [x1 y1 x2 y2] (map str->int coords)]
    (map vector (point->seq x1 x2) (point->seq y1 y2))))

(defn hor-ver?
  [s]
  (let [[_ & [x1 y1 x2 y2]] (re-matches pattern s)]
    (or (= x1 x2) (= y1 y2))))

(defn inc-matrix
  [matrix [x y]]
  (update-in matrix [x y] inc))

(defn set-points
  [matrix points]
  (reduce inc-matrix matrix points))

(defn square-matrix
  [size]
  (vec (repeat size (vec (repeat size 0)))))

(defn populate-matrix
  [input]
  (loop [lines input
         matrix (square-matrix 999)]
    (if (empty? lines)
      matrix
      (let [[row & remaining] lines
            points (parse-row row)]
        (recur remaining
               (set-points matrix points))))))

(defn overlaps
  [input]
  (let [matrix (populate-matrix input)]
    (reduce + 0 (map (fn [row] (count (filter #(> % 1) row)))
       matrix))))

(println  "sol 1:" (overlaps (filter hor-ver? input)))

(println  "sol 2:" (overlaps  input))