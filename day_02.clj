#!/usr/bin/env bb
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_02.txt")))

;; ------------------------------------ part 1

(defn forward
  [position value]
  (assoc position :horizontal (+ (:horizontal position) value)))

(defn down
  [position value]
  (assoc position :depth (+ (:depth position) value)))

(defn up
  [position value]
  (assoc position :depth (- (:depth position) value)))

(defn move
 [input]
 (let [computed (reduce (fn [position row]
                          (let [[cmd val] (str/split row #" ")
                                k (resolve (symbol cmd))
                                v (Integer/parseInt val)]
                            (apply k [position v])))
                        {:depth 0 :horizontal 0 :aim 0}
                        input)]
   (* (:depth computed) (:horizontal computed))))

(println "sol 1" (move input))

;; ------------------------------------ part 2

(defn forward
  [position value]
  (assoc position
         :horizontal (+ (:horizontal position) value) 
         :depth (+ (* value (:aim position)) (:depth position))))

(defn down
  [position value]
  (assoc position :aim (+ (:aim position) value)))

(defn up
  [position value]
  (assoc position :aim (- (:aim position) value)))

(println  "sol 2" (move input))