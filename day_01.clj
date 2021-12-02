#!/usr/bin/env bb
(require '[clojure.string :as str])

(def input (map #(Integer/parseInt %) (str/split-lines (slurp "./input/day_01.txt"))))

(def not-nil? (complement nil?))

;; ------------------------------------ part 1

(defn increments
  [input]
  (:inc (reduce #(assoc (if (and (not-nil? (:last %1)) (< (:last %1) %2))
                    (update %1 :inc inc)
                    %1)
                  :last
                  %2)
          {:last nil :inc 0}
          input)))

(println "sol 1" (increments input))

;; ------------------------------------ part 2

(defn increments
  [input]
  (:inc (reduce #(assoc (if (and (not-nil? (:last %1)) (< (:last %1) %2))
                    (update %1 :inc inc)
                    %1)
                  :last
                  %2)
          {:last nil :inc 0}
          (map #(reduce + %1) (partition 3 1 input)))))

(println "sol 2" (increments input))