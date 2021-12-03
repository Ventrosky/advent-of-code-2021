#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_03.txt")))

;; ------------------------------------ part 1

(defn input->seq
  [input]
  (map seq input))

(def report (input->seq input))

(defn same-freq
  [freq]
  (apply = (vals freq)))

(defn num->char
  [n]
  (.charAt (str n) 0))

(defn x-common
  [fu default]
  (fn [freq]
    (num->char (if (same-freq freq)
                 default
                 (key (apply fu val freq))))))

(def most-common
  (x-common max-key 1))

(def least-common
  (x-common min-key 0))

(defn bit-freqs
  [bits]
  (let [empty-value (map (fn [_] "") (first input))]
    (map frequencies (reduce #(map str %1 %2) empty-value bits))))

(defn bin->dec
  [bin]
  (Integer/parseInt (str/join "" bin) 2))

(defn gamma-rate
  [report]
  (bin->dec (map most-common (bit-freqs report))))

(defn epsilon-rate
  [report]
  (bin->dec (map least-common (bit-freqs report))))

(defn power-consumption
  [report]
  (* (gamma-rate report) (epsilon-rate report)))

(println "sol 1:" (power-consumption report))

;; ------------------------------------ part 2

(defn rating
  [compare-fu]
  (fn [report]
    (let [is-good? (fn [b i fu freq]
                     (let [f (nth freq i)
                           x (nth b i)]
                       (= (char (fu f)) x)))]
      (loop [r  report
             i 0]
        (if (<= (count r) 1)
          (bin->dec (first r))
          (recur (filter #(is-good? % i compare-fu (bit-freqs r)) r) (+ i 1)))))))

(def oxygen-generator (rating most-common))
(def co2-scrubber (rating least-common))

(defn life-support
  [report]
  (* (oxygen-generator report) (co2-scrubber report)))

(println "sol 2:" (life-support report))