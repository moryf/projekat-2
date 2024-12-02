(ns projekat-alati.core
  (:gen-class))
  (require '[clojure.math :as math])
  (require '[criterium.core :as criterium])


(defn sigmoid [x]
(/ 1 (+ 1 (math/pow Math/E (- x)))))

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

(random-weight)

(defn softmax [x]
  (let [exponencijal (map #(Math/exp %) x)
        suma (reduce + exponencijal)]
    (map #(/ % suma) exponencijal)))


(defrecord Neuron [weights activation-function])

(defn initiate-neuron [input-size activation-function]
(Neuron. (vec (repeatedly input-size random-weight)) activation-function))

(initiate-neuron 3 :sigmoid)

(defrecord Layer [neurons])

(defn initiate-layer [input-size layer-size activation-function]
(Layer. (vec (repeatedly layer-size #(initiate-neuron input-size activation-function)))))

(initiate-layer 3 2 :sigmoid)

(defrecord Network [layers])

(defn initiate-network [layers]
(Network. layers))

(initiate-network [
  (initiate-layer 2 2 :sigmoid)
  (initiate-layer 2 10 :sigmoid)
  (initiate-layer 10 3 :sigmoid)
])

(defn neuron-output [neuron inputs]
  (let [output (map * (:weights neuron) inputs)
    sum (reduce + (map * (:weights neuron) inputs))]
    (case (:activation-function neuron)
    :sigmoid (sigmoid sum)
    :tanh (tanh sum))))

(defn layer-output [inputs layer]
(map #(neuron-output % inputs) (:neurons layer)))

(criterium/with-progress-reporting (criterium/quick-bench (layer-output (initiate-layer 2 100 :sigmoid) [1 2])))

(defn network-output [network inputs]
  (softmax (reduce (fn [layer inputs]
                     (layer-output inputs layer))
                   inputs
                   (:layers @network))))


(criterium/with-progress-reporting(criterium/quick-bench (network-output (initiate-network [(initiate-layer 2 50 :sigmoid)
                                   (initiate-layer 50 50 :sigmoid)
                                   (initiate-layer 50 3 :sigmoid)]) [1 2 4])))

(defn mean-squared-error [network inputs targets]
  (let [outputs (network-output network inputs)]
    (reduce + (map #(* % %) (map - outputs targets)))))


(defn create-network-atom [layers]
  (atom (initiate-network layers)))

(def network-atom (create-network-atom [(initiate-layer 2 2 :sigmoid)
                                        (initiate-layer 2 10 :sigmoid)
                                        (initiate-layer 10 3 :sigmoid)]))




