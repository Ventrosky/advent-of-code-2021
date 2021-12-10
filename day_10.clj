#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_10.txt")))

;; ------------------------------------ part 1

(def parens { ")" "(" "}" "{" "]" "[" ">" "<"})
(def scores {")" {:wrong 3 :missing 1} "]" {:wrong 57 :missing 2} "}" {:wrong 1197 :missing 3 } ">" {:wrong 25137 :missing 4}})

(defn swap-keys
  [coll]
  (reduce-kv #(assoc %1 %3 %2) {} coll))

(defn find-invalid
  [braces]
  (loop [row (str/split braces #"")
         stack '()]
    (let [b (first row)
          r (rest row)
          closing (get parens b)
          r-parens (swap-keys parens)]
      (cond
        (empty? row) {:missing (map #(get r-parens %) stack )}
        (nil? closing) (recur r (conj stack b))
        (= closing (first stack)) (recur r (rest stack))
        :else  {:wrong b }))))

(defn valid-braces 
  [input]
  (reduce #(+ %1 (or (:wrong (get scores (:wrong %2))) 0)) 0 (map find-invalid input)))

(println "sol 1:" (valid-braces input))

;; ------------------------------------ part 2

(defn add-score
  [tot n]
  (+ (* 5 tot) n))

(defn score-row 
  [row]
  (let [s (map  #(:missing (get scores %)) row)]
    (reduce add-score 0 s)))
  
(defn missing-braces
  [input]
  (let [incomplete (filter identity (map #(:missing (find-invalid %)) input))
        row-scores (sort (map score-row incomplete))]
     (nth row-scores (/ (count row-scores) 2))))

(println "sol 2:" (missing-braces input))