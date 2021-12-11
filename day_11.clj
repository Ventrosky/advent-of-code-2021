#!/usr/bin/env bb

(def input (slurp "./input/day_11.txt"))

(def SIZE 10)

(defn parse-state
  [input]
  (let [octopi (re-seq #"\d" input)]
    (zipmap (for [i (range SIZE) j (range SIZE)] [i j])
            (map #(Integer/parseInt %) octopi))))

(defn in-bounds?
  [point]
  (let [check #(<= 0 %1 (dec SIZE))]
    (every? check point)))

(defn neighbours 
  [[[x y] _]]
  (filter in-bounds?
          (for [i '(-1, 0, 1) j '(-1, 0, 1)]
            [(+ x i) (+ y j)])))

(defn inc-energy
  [state]
  (into {} (for [[k v] state] [k (inc v)])))

(defn find-flashing
  [state]
  (filter #(> (second %) 9) state))

(defn update-values [m v f]
  (reduce #(update-in % [%2] f) m v))

(defn inc-neighbours 
  [state n]
  (update-values state (mapcat #(neighbours  %) n) #(if (zero? %) 0 (inc %))))

(defn reset-flashed
  [state n]
  (update-values state (map first n) (constantly 0)))

(defn do-flash 
  [state]
  (loop [state (inc-energy state)
         f (find-flashing state)]
    (if (empty? f)
      state
      (let [state (reset-flashed (inc-neighbours state f) f)]
        (recur state (find-flashing state))))))

(defn next-step
  [state]
  (let [state (do-flash state)]
    state))

(defn generator
  [input]
  (let [state (next-step (parse-state input))]
    (iterate next-step state)))

(defn count-flash
  [state]
  (let [flash? #(zero? (second %))]
    (count (filter flash? state))))

(defn count-flashes
  [input steps]
  (reduce #(+ %1 (count-flash %2))
          0
          (take steps (generator input))))


(println "sol 1:" (count-flashes input 100))

;; ------------------------------------ part 2

(defn simultaneous
  [input]
  (loop [state (parse-state input)
         step 0]
    (if (= (count-flash state) 100)
      step
      (recur (next-step state) (inc step)))))

(println "sol 2:" (simultaneous input))