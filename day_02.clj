#!/usr/bin/env bb
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_02.txt")))

;; ------------------------------------ part 1

(defn pos-updater
  [key fu]
  (fn [position value]
    (assoc position key (fu (key position) value))))

(def forward (pos-updater :horizontal +))

(def down (pos-updater :depth +))

(def up (pos-updater :depth -))

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

(println "sol 1:" (move input))

;; ------------------------------------ part 2

#_{:clj-kondo/ignore [:redefined-var]}
(defn forward
  [position value]
  (assoc position
         :horizontal (+ (:horizontal position) value) 
         :depth (+ (* value (:aim position)) (:depth position))))

#_{:clj-kondo/ignore [:redefined-var]}
(def down (pos-updater :aim +))

#_{:clj-kondo/ignore [:redefined-var]}
(def up (pos-updater :aim -))

(println  "sol 2:" (move input))