(ns waterpouring.records
  (:require [waterpouring.core :refer [initialise-state glasses]]))

(defprotocol
    Move
  (change-state [this state]))

(defrecord Empty [glass]
  Move
  (change-state [this state] (if (< glass (count state))
                               (assoc-in state [glass :vol] 0)
                               state)))

(defrecord Fill [glass]
  Move
  (change-state [this state] (if (< glass (count state))
                               (update state glass #(assoc % :vol (:capacity %)))
                               state)))

(defrecord Pour [from to]
  Move
  (change-state [this state] (let [no-of-glasses (count state)]
                               (if (and (< from no-of-glasses) (< to no-of-glasses))
                                 (let [change-vol (min (get-in state [from :vol])
                                                       (- (get-in state [to :capacity])
                                                          (get-in state [to :vol])))]
                                   (-> state
                                       (update-in [to :vol] + change-vol)
                                       (update-in [from :vol] - change-vol)))
                                 state))))
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


(defn has-target-vol?
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

(comment

  (take 1 (solutions [1 2] 2))

  (take 1 (solutions [2 4 7] 1))

  )
