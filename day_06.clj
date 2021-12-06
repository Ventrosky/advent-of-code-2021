#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (slurp "./input/day_06.txt"))

(defn str->int
  [s]
  (Integer/parseInt s))

(defn rotate
  [state]
  (vec (concat (drop 1 state) (take 1 state))))

(defn next-gen
  [state]
  (let [state (rotate state)]
    (update state 6 #(+ (get state 8) %1))))

(defn generator
  [input]
  (let [lanternfish (map str->int (str/split input #","))
        state (reduce #(update %1 %2 inc) (vec (repeat 9 0)) lanternfish)]
    (iterate next-gen state)))

(def fish-life (generator input))

(defn count-fish
  [day]
  (reduce + (nth fish-life day)))

(println "sol 1:" (count-fish 80))

(println "sol 2:" (count-fish 256))