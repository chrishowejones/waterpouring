(ns waterpouring.maps
  (:require [waterpouring.core :refer [glasses initialise-state]]))

(defmulti state->state
  (fn [state move] (when move (:move move))))

(defmethod state->state
  :empty
  [state {:keys [glass]}] (if (< glass (count state))
                            (assoc-in state [glass :vol] 0)
                            state))

(defmethod state->state
  :fill
  [state {:keys [glass]}] (if (< glass (count state))
                            (update state glass #(assoc % :vol (:capacity %)))
                            state))

(defmethod state->state
  :pour
  [state {:keys [from to]}]
  (if (and (< from (count state))
           (< to (count state)))
   (let [change-vol (min (get-in state [from :vol])
                         (- (get-in state [to :capacity])
                            (get-in state [to :vol])))]
     (-> state
         (update-in [to :vol] + change-vol)
         (update-in [from :vol] - change-vol)))
   state))

(defmethod state->state
  :default
  [state move] (do (println "Move " move) state))

(defn moves
  [glasses]
  (into
   (mapcat (juxt (partial hash-map :move :empty :glass)
                 (partial hash-map :move :fill :glass)) glasses)
   (for [from glasses to glasses :when (not= to from)] {:move :pour :from from :to to})))

(defn extendpath [path move]
  (conj path move))

(defn endstate [path initial-state]
  (reduce state->state initial-state path))

(defn- generate-paths-fn
  [capacity]
  (fn [paths-set]
    (reduce
     (fn [acc-set path] (into acc-set (map (partial extendpath path) (moves (glasses capacity)))))
     #{} paths-set) ))

(defn from-paths [initial-paths capacity]
  (let [generate-paths (generate-paths-fn capacity)]
    (iterate generate-paths initial-paths)))

(defn has-target-vol?
  [target initial-state]
  (fn [path]
    (some (partial = target) (map :vol (endstate path initial-state)))))

(defn solutions [capacity target]
  (let [initial-state (initialise-state capacity)
        fp (from-paths #{[]} capacity)]
    (mapcat #(filter (has-target-vol? target initial-state) %) fp)))

(comment

  (take 2 (solutions [1 2] 2))

  (take 3 (solutions [2 4 7] 3))

  )
