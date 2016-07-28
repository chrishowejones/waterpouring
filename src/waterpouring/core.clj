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

(defn glasses [capacities]
  (range (count capacities)))
