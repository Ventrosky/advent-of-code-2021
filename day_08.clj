#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.set :as s])

(def input (str/split-lines (slurp "./input/day_08.txt")))

;; ------------------------------------ part 1

(defn digits-1-4-7-8
  [input]
  (let [parser #(str/split (second (str/split % #" \| ")) #" ")]
    (count (filter #(contains? #{2 4 3 7} (count %)) (flatten (map parser input))))))

(println "sol 1:" (digits-1-4-7-8 input))

;; ------------------------------------ part 2

(defn str->int
  [s]
  (Integer/parseInt s))

(def not-subset? (complement s/subset?))

(defn get-code
  [codes i]
  (first (codes i)))

(defn finder
  [n]
  (fn [coll] 
    (filter #(when (= (count %) n) %) coll)))

(defn find-03
  [x1 x2 codes]
  (filter #(and (not= x1 %) (not= x2 %)) codes))

(defn find-2
  [x1 x2 codes]
  (filter #(and (= (count (s/difference x1 %)) 1) (not= x2 %)) codes))

(defn find-5
  [x codes]
  (filter #(= (count (s/difference x %)) 1) codes))

(defn find-6
  [x codes]
  (filter #(not-subset? x %) codes))

(defn find-9
  [x codes]
  (filter #(= x %) codes))

(defn swap-keys
  [coll]
  (reduce-kv #(assoc %1 %3 %2) {} coll))

(defn decode
  [[digits output]]
  (let [s-digits (map set digits)
        s-output (map #(str/join (sort %)) output)
        s-len '(6 2 5 5 4 5 6 3 7 6)
        initial (vec (map #(%1 %2) (map finder s-len) (repeat 10 s-digits)))
        step1 #(update-in % [6] (partial find-6 (get-code % 1)))
        step2 #(update-in % [5] (partial find-5 (get-code % 6)))
        step3 #(update-in % [9] (partial find-9 (s/union (get-code % 1) (get-code % 5))))
        step4 #(update-in % [0] (partial find-03 (get-code % 6) (get-code % 9)))
        step5 #(update-in % [2] (partial find-2 (get-code % 1) (get-code % 5)))
        step6 #(update-in % [3] (partial find-03 (get-code % 2) (get-code % 5)))
        convert (fn [coll] (str->int (reduce #(str/join [%1 (get coll %2)]) "" s-output)))]
    (->>
     initial
     (step1)
     (step2)
     (step3)
     (step4)
     (step5)
     (step6)
     (map #(str/join (sort (first %))))
     (vec)
     (swap-keys)
     (convert))))

(defn parse-all
  [input]
  (map #(str/split % #" ") (str/split input #" \| ")))

(defn add-output
  [input]
  (let [signals (map parse-all input)]
    (reduce + (map decode signals))))

(println "sol 2:" (add-output input))