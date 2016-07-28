(ns waterpouring.records-test
  (:require [clojure.test :refer [deftest is testing]]
            [waterpouring.records :refer :all]))

(deftest fill-change-state
  (let [state [{:vol 0 :capacity 3}
               {:vol 2 :capacity 4}
               {:vol 2 :capacity 2}
               {:vol 0 :capacity 0}]]
   (testing "should fill glass if empty"
     (is (= 3 (:vol (first (change-state (->Fill 0) state))))))
   (testing "should fill glass if partially filled"
     (is (= 4 (:vol (second (change-state (->Fill 1) state))))))
   (testing "should do nothing if glass full"
     (is (= 2 (:vol ((change-state (->Fill 2) state) 2))))
     (is (zero? (:vol ((change-state (->Fill 3) state) 3)))))
   (testing "should do nothing if glass out of range"
     (is (= state (change-state (->Fill 4) state))))))

(deftest empty-change-state
  (let [state [{:vol 0 :capacity 3}
               {:vol 2 :capacity 4}
               {:vol 2 :capacity 2}
               {:vol 0 :capacity 0}]]
    (testing "should empty glass if full"
      (is (zero? (:vol (first (change-state (->Empty 2) state))))))
    (testing "should empty glass if partially filled"
      (is (zero? (:vol (second (change-state (->Empty 1) state))))))
    (testing "should do nothing if glass empty"
      (is (zero? (:vol ((change-state (->Empty 3) state) 3)))))
    (testing "should do nothing if glass out of range"
      (is (= state (change-state (->Empty 4) state))))))

(deftest pour-change-state
  (let [state [{:vol 3 :capacity 3}
               {:vol 0 :capacity 4}
               {:vol 0 :capacity 2}
               {:vol 2 :capacity 3}
               {:vol 0 :capacity 0}]]
    (testing "glass vol should end up in target glass if target glass emtpy"
      (let [state-after (change-state (->Pour 0 1) state)]
        (is (= 3 (:vol (second state-after))))
        (is (zero? (:vol (first state-after))))))
    (testing "glass vol should reduce in source glass by capacity of target glass if target glass has less capacity than vol in source"
      (let [state-after (change-state (->Pour 0 2) state)]
        (is (= 1 (:vol (first state-after))))
        (is (= 2 (:vol (state-after 2)))))
      (let [state-after (change-state (->Pour 0 3) state)]
        (is (= 2 (:vol (first state-after))))
        (is (= 3 (:vol (state-after 3))))))
    (testing "should do nothing if target glass capacity zero"
      (let [state-after (change-state (->Pour 0 4) state)]
        (is (= state state-after))))
    (testing "should do nothing if source glass empty"
      (let [state-after (change-state (->Pour 1 2) state)]
        (is (= state state-after))))
    (testing "should do nothing if target glass empty"
      (let [state-after (change-state (->Pour 2 1) state)]
        (is (= state state-after))))
    (testing "should do nothing if either glass out of range"
      (is (= state (change-state (->Pour 0 5) state)))
      (is (= state (change-state (->Pour 5 0) state)))
      (is (= state (change-state (->Pour 5 4) state))))))

(deftest test-extend-path
  (testing "should add a move to the given path if path has empty history of moves"
    (let [path-after (extend-path (->Path []) (->Pour 0 1))]
      (is (= 1 (count (:history path-after))))
      (is (= (->Pour 0 1) (first (:history path-after))))))
  (testing "should add a move to the given path if path already has history of moves"
    (let [path-after (extend-path (->Path [(->Fill 0)]) (->Pour 0 1))]
      (is (= 2 (count (:history path-after))))
      (is (= (->Pour 0 1) (second (:history path-after)))))))

(deftest test-end-state
  (testing "end state should result in no change of state if the path has no moves"
    (let [state [{:vol 3 :capacity 3}
                 {:vol 0 :capacity 4}
                 {:vol 0 :capacity 2}
                 {:vol 2 :capacity 3}
                 {:vol 0 :capacity 0}]]
      (is (= state (end-state (->Path []) state)))))
  (testing "end state should reflect application of single move in path"
    (let [state [{:vol 3 :capacity 3}
                 {:vol 0 :capacity 4}
                 {:vol 0 :capacity 2}
                 {:vol 2 :capacity 3}
                 {:vol 0 :capacity 0}]
          state-expected [{:vol 0 :capacity 3}
                          {:vol 3 :capacity 4}
                          {:vol 0 :capacity 2}
                          {:vol 2 :capacity 3}
                          {:vol 0 :capacity 0}]]
      (is (= state-expected (end-state (->Path [(->Pour 0 1)]) state)))))
  (testing "end state should reflect application of several moves in path"
    (let [state [{:vol 3 :capacity 3}
                 {:vol 0 :capacity 4}
                 {:vol 0 :capacity 2}
                 {:vol 2 :capacity 3}
                 {:vol 0 :capacity 0}]
          state-expected [{:vol 0 :capacity 3}
                          {:vol 3 :capacity 4}
                          {:vol 2 :capacity 2}
                          {:vol 0 :capacity 3}
                          {:vol 0 :capacity 0}]]
      (is (= state-expected (end-state (->Path [(->Pour 0 1) (->Fill 2) (->Empty 3) (->Pour 1 4)]) state))))))

(deftest check-has-target-vol?
  (testing "should return truthy when the target volume is the end state of the path"
    (let [path (->Path [(->Fill 2) (->Fill 1) (->Empty 1) (->Pour 2 0)])]
      (is ((waterpouring.records/has-target-vol? 5 [{:vol 0 :capacity 2}
                                                    {:vol 0 :capacity 4}
                                                    {:vol 0 :capacity 7}]) path))))
  (testing "should return falsey when the target volume is the end state of the path"
    (let [path (->Path [(->Fill 2) (->Fill 1) (->Empty 1)])]
      (is (not ((waterpouring.records/has-target-vol? 5 [{:vol 0 :capacity 2}
                                                         {:vol 0 :capacity 4}
                                                         {:vol 0 :capacity 7}]) path))))))
