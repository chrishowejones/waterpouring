(ns waterpouring.core
  (:gen-class)
  (:require [clojure.set :as set]))

;; state is a vector of maps with each map representing a glass that has
;; a capacity and a current volume of water.

(defn initialise-state
  "Takes a sequence of capacities and generates a vector glass 'maps' with the requisite capacities
  and intial volume of zero"
  [capacities]
  (mapv #(hash-map :vol 0 :capacity %) capacities))

(defprotocol
    Move
  (change-state [this state]))

(defrecord Empty [glass]
  Move
  (change-state [this state] (assoc-in state [glass :vol] 0)))

(defrecord Fill [glass]
  Move
  (change-state [this state] (update state glass #(assoc % :vol (:capacity %)))))

(defrecord Pour [from to]
  Move
  (change-state [this state] (let [change-vol (min (get-in state [from :vol])
                                                   (- (get-in state [to :capacity])
                                                      (get-in state [to :vol])))]
                               (-> state
                                   (update-in [to :vol] + change-vol)
                                   (update-in [from :vol] - change-vol)))))

(defn glasses [capacities]
  (range (count capacities)))

(defn moves
  [glasses]
  (into  (mapcat (juxt ->Empty ->Fill) glasses)
         (for [to glasses
               from glasses
               :when (not= to from)]
           (->Pour from to))))

(defprotocol
    Paths
    (extend-path [this move])
    (end-state [this initial-state]))

(defrecord Path
    [history]
    Paths
    (extend-path [this move]
      (->Path (conj history move)))
    (end-state [this initial-state] (reduce (fn [state move] (change-state move state)) initial-state history)))

(defn from-fn
  [capacity]
  (fn from [paths]
    "Takes a set of paths and extends them by possible moves to return a seq of
   sets of paths provided the path has not already been explored."
    (if (empty? paths)
      []
      (lazy-seq
       (cons paths
              (from
               (set
                (flatten
                 (for [path paths]
                   (map (partial extend-path path) (moves (glasses capacity))))))))))))

(defn- has-target-vol?
  [target initial-state]
  (fn [path]
   (some #(= % target) (map :vol (end-state path initial-state)))))

(defn solutions [capacity target]
  {:pre [(and (pos? target) (<= target (reduce max capacity)))]}
  (let [f (from-fn capacity)]
    (flatten
     (let [initial-state (initialise-state capacity)]
       (for [path-set (f #{(->Path [])})]
         (filter (has-target-vol? target initial-state) path-set))))))

(take 1 (solutions [1 2] 2))

(take 1 (solutions [2 4 7] 1))
