#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./input/day_04.txt")))

(defn str->int
  [s]
  (Integer/parseInt s))

(def draw-numbers (map str->int (str/split (first input) #",")))

(def board-input (drop 1 input))

(def size (count (str/split (str/trim (second board-input)) #"\s+")))

(def not-empty? (complement empty?))

(defn counter
  [size]
  (zipmap (take size (range)) (repeat size 0)))

(defn next-board
  [board-input]
  (let [board (take-while not-empty? board-input)
        score-row (counter size)
        score-col (counter size)
        remains (drop (inc size) board-input)
        split-row #(reduce-kv (fn [acc k v] (assoc acc (str->int v) {:col k :row %1})) {} (str/split (str/trim %2) #"\s+"))]
    (list (into {:score {:row score-row :col score-col}} (map-indexed split-row board)) remains)))

(defn split-boards 
  [[_ & lines]]
  (loop [result []
         lines lines]
    (if (not-empty? lines)
      (let [[board remains] (next-board lines)]
        (recur (conj result board) remains))
      result)))

(defn update-board
  [board n]
  (if (contains? board n)
      (let [{r :row c :col} (get board n)
            update-mark (fn [m] (update m n #(assoc % :mark true)))
            update-inc #(update-in %1 [:score %2 %3] inc)
            update-score (fn [m] (update-inc (update-inc (update-mark m) :row r) :col c))]
         (update-score board))
    board))

(defn is-end?
  [board]
  (let [{r :row c :col} (:score board)
        filled? (fn [r] (some #(if (= size %) board) (vals r)))]
     (or (filled? r) (filled? c))))

(def is-game?
  (complement is-end?))

(defn score
  [board x]
  (* (reduce-kv (fn [acc k v]
                  (if (or (contains? v :mark) (= k :score))
                    acc
                    (+ acc k))) 0 board)
     x))

(defn run-numbers
  [numbers board-input]
  (loop [numbers numbers
         state (split-boards board-input)
         prev 0
         scores []]
    (if (empty? numbers)
      scores
      (let [[x & numbers] numbers
            state (map #(update-board % x) state)]
        (recur numbers (filter is-game? state) x (into scores (map #(score % x) (filter is-end? state))) )))))

(def board-scores (run-numbers draw-numbers board-input))

(println "sol 1:" (first board-scores))

(println "sol 2:" (last board-scores))
