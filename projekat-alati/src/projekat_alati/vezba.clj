(ns projekat-alati.vezba)
(require '[criterium.core :as criterium])


(defn split-by-type [sequence-to-split]
  (let [types (set (map type sequence-to-split))]
    (map (fn [x] (filter #(= x (type %)) sequence-to-split)) types)))

(defn split-by-type-2 [s]
  (let [types (hash-map)]
    (vals (reduce (fn [acc x]
                    (let [t (type x)]`
                      (if (contains? acc t)
                        (assoc acc t (conj (acc t) x))
                        (assoc acc t [x]))))
                  types
                  s))))

(defn split-by-type-3 [s]
  (vals (group-by type s)))

(def sample (take 1000000 (random-sample  0.1 (cycle ["String" "String 5" 1 1.7 18 :a :c \c (int 6) (double 6.8) (Double. 6.6) [1 2 3] [7 5 6] '(1 2 3)]))))


(defn split-by-type-4 [input]
  (loop [l input m {}]
    (if (empty? l)
      (set (vals m))
      (let [f (first l)
            r (rest l)
            e (m (type f))
            x (if (nil? e)
                [(type f) [f]]
                [(type f) (conj e f)])]
        (recur r (conj m x))))))

(defn split-by-type-5 [col]
  (loop [typeset (set (map type col)) result []]
    (if (empty? typeset)
      result
      (recur (rest typeset) (conj result (filter #(= (type %) (first typeset)) col))))))

(defn split-by-type-6 [xs] (set (map (fn [t] (filter #(= (type %) t) xs)) (distinct (map type xs)))))

(time (split-by-type sample))
(time (split-by-type-2 sample))
(time (split-by-type-3 sample))

(criterium/with-progress-reporting (criterium/quick-bench (split-by-type sample)))
(criterium/with-progress-reporting (criterium/quick-bench (split-by-type-2 sample)))
(criterium/with-progress-reporting (criterium/quick-bench (split-by-type-3 sample)))
(criterium/with-progress-reporting (criterium/quick-bench (split-by-type-4 sample)))
(criterium/with-progress-reporting (criterium/quick-bench (split-by-type-5 sample)))
(criterium/with-progress-reporting (criterium/quick-bench (split-by-type-6 sample)))

(count (split-by-type-6 sample))
(count (split-by-type-5 sample))
(count (split-by-type-4 sample))
(count (split-by-type-3 sample))
(count (split-by-type-2 sample))
(count (split-by-type sample))

(map count (split-by-type sample))
(map count (split-by-type-2 sample))
(map count (split-by-type-3 sample))
(map count (split-by-type-4 sample))
(map count (split-by-type-5 sample))
(map count (split-by-type-6 sample))