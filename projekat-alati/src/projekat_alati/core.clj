(ns projekat-alati.core
  (:gen-class))
  (require '[clojure.math :as math])


(defn sigmoid [x]
(/ 1(+ 1 (math/pow Math/E (- x)))))

(defn sigmoid-derivitive [x]
(* (sigmoid x) (- 1 (sigmoid x))))

(defn tanh [x]
(/ (- (math/pow Math/E (* 2 x)) 1) (+ (math/pow Math/E (* 2 x)) 1)))

(defn tanh-derivative [x]
  (- 1 (math/pow (tanh x) 2)))

(defn random-weight []
  (- (* 2 (rand)) 1))

(sigmoid 1)

(sigmoid-derivitive 0.7310585786300049)

(tanh 1)

(tanh-derivative 0.761594155955765)


