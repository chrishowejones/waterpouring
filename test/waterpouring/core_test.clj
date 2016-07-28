(ns waterpouring.core-test
  (:require [clojure.test :refer :all]
            [waterpouring.core :refer :all]))

(deftest check-initialise-state
  (testing "should return vector of glass maps with vol of zero and specified capacities"
    (is (= [{:vol 0 :capacity 2}
            {:vol 0 :capacity 4}
            {:vol 0 :capacity 7}]
           (initialise-state [2 4 7]))))
  (testing "should return empty vector if capacities vector empty"
    (is (empty? (initialise-state []))))
  (testing "should return empty vector if capacities vector nil"
    (is (empty? (initialise-state nil)))))

(deftest check-glasses
  (testing "should generate a sequence of indexes for all the glasses specified in the
            capacities vector"
    (is (= [0 1 2] (glasses [2 4 7]))))
  (testing "should generate an empty sequence if given empty capacities vector"
    (is (empty? (glasses []))))
  (testing "should generate an empty sequence if given nil capacities"
    (is (empty? (glasses nil)))))
