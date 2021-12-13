#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (slurp "./input/day_13.txt"))

(def ^:const FILLED "#")
(def ^:const EMPTY ".")

(defn str->int
  [s]
  (Integer/parseInt s))

(defn pair->coord
  [[_ x y]]
  {:x (str->int x) :y (str->int y)})

(defn fold->step
  [[_ axis v]]
  [(keyword axis) (str->int v)])

(defn extract-matches
  [pattern s fu]
  (mapv fu (re-seq pattern  s)))

(defn parse-input
  [input]
  (let [[p f] (str/split input #"\n\n")
        coords (extract-matches #"(\d+),(\d+)"  p pair->coord)
        steps (extract-matches #"(x|y)=(\d+)" f fold->step)]
    {:coords (zipmap coords (repeat FILLED)) :steps steps}))

(defn convert
  [[point _] [axis v]]
  (let [p (axis point)]
    (if (> p v) 
      (assoc point axis (- (* v 2) p))
      point)))

(defn fold
  [coords step]
  (reduce #(assoc %1 (convert %2 step) FILLED) {} coords))

(defn fold-manual
  [input & {:keys [p-step] :or {p-step identity}}]
  (let [{coords :coords steps :steps} (parse-input input)]
    (reduce #(fold %1 %2) coords (p-step steps))))

(defn count-first-fold
  [input]
  (count (fold-manual input :p-step #(take 1 %))))

(println "sol 1:" (count-first-fold input))

(defn print-manual
  [input]
  (let [coords (fold-manual input)
        width (apply max (map :x (keys coords)))
        height (apply max (map :y (keys coords)))]
    (doseq [y (range (inc height)) x (range (inc width))]
      (print (or (get coords {:x x :y y}) EMPTY))
      (when (= x width) (println)))))

(println "sol 2:")
(print-manual input)