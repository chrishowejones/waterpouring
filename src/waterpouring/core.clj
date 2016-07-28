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

(defmulti state->state
  (fn [state move] (when move (:move move))))

(defmethod state->state
  :empty
  [state {:keys [glass]}] (assoc-in state [glass :vol] 0))

(defmethod state->state
  :fill
  [state {:keys [glass]}] (update state glass #(assoc % :vol (:capacity %))))

(defmethod state->state
  :pour
  [state {:keys [from to]}]
  (let [change-vol (min (get-in state [from :vol])
                        (- (get-in state [to :capacity])
                           (get-in state [to :vol])))]
    (-> state
        (update-in [to :vol] + change-vol)
        (update-in [from :vol] - change-vol))))

(defmethod state->state
  :default
  [state move] (do (println "Move " move) state))


(defn glasses [capacities]
  (range (count capacities)))

(defn new-moves
  [glasses]
  (into
   (mapcat (juxt (partial hash-map :move :empty :glass)
                 (partial hash-map :move :fill :glass)) glasses)
   (for [from glasses to glasses :when (not= to from)] {:move :pour :from from :to to})))

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

(defn extendpath [path move]
  (conj path move))

(defn endstate [path initial-state]
  (reduce state->state initial-state path))

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

(defn- generate-paths-fn
  [capacity]
  (fn [paths-set]
    (reduce
     (fn [acc-set path] (into acc-set (map (partial extendpath path) (new-moves (glasses capacity)))))
     #{} paths-set) ))

(defn from-paths [initial-paths capacity]
  (let [generate-paths (generate-paths-fn capacity)]
    (iterate generate-paths initial-paths)))

((generate-paths-fn [1 2]) #{[]})

(take 2 (from-paths #{[]} [1 2]))

(defn- has-target-vol?
  [target initial-state]
  (fn [path]
    (some #(= % target) (map :vol (end-state path initial-state)))))

(defn- has-target-vol2?
  [target initial-state]
  (fn [path]
    (some (partial = target) (map :vol (endstate path initial-state)))))

((has-target-vol2? 2 (initialise-state [1 2])) [{:move :empty :glass 1}])

(defn solutions [capacity target]
  {:pre [(and (pos? target) (<= target (reduce max capacity)))]}
  (let [f (from-fn capacity)]
    (flatten
     (let [initial-state (initialise-state capacity)]
       (for [path-set (f #{(->Path [])})]
         (filter (has-target-vol? target initial-state) path-set))))))

(defn sols [capacity target]
  (let [initial-state (initialise-state capacity)
        fp (from-paths #{[]} capacity)]
    (mapcat #(filter (has-target-vol2? target initial-state) %) fp)))

(let [ps (take 1 (drop 1 (sols [1 2] 2)))]
  (filter (constantly true) ps))

(take 2 (sols [1 2] 2))

(take 3 (sols [2 4 7] 3))

#_(-> (state->state (initialise-state [1 2]) [])
    (state->state [{:move :fill :glass 0}]))

(take 1 (solutions [1 2] 2))

(take 1 (solutions [2 4 7] 1))
