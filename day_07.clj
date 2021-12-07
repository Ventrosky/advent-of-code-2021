#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (slurp "./input/day_07.txt"))

;; ------------------------------------ part 1

(defn str->int
  [s]
  (Integer/parseInt s))

(defn counter
  [size]
  (zipmap (take size (range)) (repeat size 0)))

(defn distance
  [i j]
  (let [diff  (- i j)]
    (max diff (- diff))))

(defn cheapest-fuel
  [input & {:keys [rate] :or {rate identity}}]
  (let [positions (vec (map str->int (str/split input #",")))
        crabs-num (count positions)
        find-fuel #(apply min (vals % ))]
    (find-fuel (reduce (fn [dicto [i j]]
                         (update dicto i #(+ % (rate (distance (positions j) i)))))
                       (counter crabs-num)
                       (for [i (range crabs-num) j (range crabs-num)]
                         [i j])))))

(println "sol 1:" (cheapest-fuel input))

;; ------------------------------------ part 2

(defn nth-triangle
  [n]
  (/ (* n (inc n)) 2))

(println "sol 2:" (cheapest-fuel input :rate nth-triangle))