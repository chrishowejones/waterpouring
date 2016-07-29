(ns waterpouring.maps-test
  (:require [waterpouring.maps :refer :all]
            [clojure.test :refer :all]))

(deftest fill-change-state
  (let [state [{:vol 0 :capacity 3}
               {:vol 2 :capacity 4}
               {:vol 2 :capacity 2}
               {:vol 0 :capacity 0}]]
   (testing "should fill glass if empty"
      (is (= 3 (:vol (first (state->state state {:move :fill :glass 0}))))))
   (testing "should fill glass if partially filled"
     (is (= 4 (:vol (second (state->state state {:move :fill :glass 1}))))))
   (testing "should do nothing if glass full"
     (is (= 2 (:vol ((state->state state {:move :fill :glass 2}) 2))))
     (is (zero? (:vol ((state->state state {:move :fill :glass 3}) 3)))))
   (testing "should do nothing if glass out of range"
     (is (= state (state->state state {:move :fill :glass 4}))))))

(deftest empty-state->state
  (let [state [{:vol 0 :capacity 3}
               {:vol 2 :capacity 4}
               {:vol 2 :capacity 2}
               {:vol 0 :capacity 0}]]
    (testing "should empty glass if full"
      (is (zero? (:vol (first (state->state state {:move :empty :glass 2}))))))
    (testing "should empty glass if partially filled"
      (is (zero? (:vol (second (state->state state {:move :empty :glass 1}))))))
    (testing "should do nothing if glass empty"
      (is (zero? (:vol ((state->state state {:move :empty :glass 3}) 3)))))
    (testing "should do nothing if glass out of range"
      (is (= state (state->state state {:move :empty :glass 4}))))))

(deftest pour-state->state
  (let [state [{:vol 3 :capacity 3}
               {:vol 0 :capacity 4}
               {:vol 0 :capacity 2}
               {:vol 2 :capacity 3}
               {:vol 0 :capacity 0}]]
    (testing "glass vol should end up in target glass if target glass emtpy"
      (let [state-after (state->state state {:move :pour :from 0 :to 1})]
        (is (= 3 (:vol (second state-after))))
        (is (zero? (:vol (first state-after))))))
    (testing "glass vol should reduce in source glass by capacity of target glass if target glass has less capacity than vol in source"
      (let [state-after (state->state state {:move :pour :from 0 :to 2})]
        (is (= 1 (:vol (first state-after))))
        (is (= 2 (:vol (state-after 2)))))
      (let [state-after (state->state state {:move :pour :from 0 :to 3})]
        (is (= 2 (:vol (first state-after))))
        (is (= 3 (:vol (state-after 3))))))
    (testing "should do nothing if target glass capacity zero"
      (let [state-after (state->state state {:move :pour :from 0 :to 4})]
        (is (= state state-after))))
    (testing "should do nothing if source glass empty"
      (let [state-after (state->state state {:move :pour :from 1 :to 2})]
        (is (= state state-after))))
    (testing "should do nothing if target glass empty"
      (let [state-after (state->state state {:move :pour :from 2 :to 1})]
        (is (= state state-after))))
    (testing "should do nothing if either glass out of range"
      (is (= state (state->state state {:move :pour :from 0 :to 5})))
      (is (= state (state->state state {:move :pour :from 5 :to 0})))
      (is (= state (state->state state {:move :pour :from 5 :to 4}))))))

(deftest test-extendpath
  (testing "should add a move to the given path if path has empty history of moves"
    (let [path-after (extendpath [] {:move :pour :from 0 :to 1})]
      (is (= 1 (count path-after)))
      (is (= {:move :pour :from 0 :to 1} (first path-after)))))
  (testing "should add a move to the given path if path already has history of moves"
    (let [path-after (extendpath [{:move :fill :glass 0}] {:move :pour :from 0 :to 1})]
      (is (= 2 (count path-after)))
      (is (= {:move :pour :from 0 :to 1} (second path-after))))))

(deftest test-endstate
  (testing "end state should result in no change of state if the path has no moves"
    (let [state [{:vol 3 :capacity 3}
                 {:vol 0 :capacity 4}
                 {:vol 0 :capacity 2}
                 {:vol 2 :capacity 3}
                 {:vol 0 :capacity 0}]]
      (is (= state (endstate [] state)))))
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
      (is (= state-expected (endstate [{:move :pour :from 0 :to 1}] state)))))
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
      (is (= state-expected (endstate [{:move :pour :from 0 :to 1} {:move :fill :glass 2}  {:move :empty :glass 3} {:move :pour :from 1 :to 4}] state))))))

(deftest check-has-target-vol?
  (testing "should return truthy when the target volume is the end state of the path"
    (let [path [{:move :fill :glass 2} {:move :fill :glass 1}  {:move :empty :glass 1} {:move :pour :from 2 :to 0}]]
      (is ((has-target-vol? 5 [{:vol 0 :capacity 2}
                                                    {:vol 0 :capacity 4}
                                                    {:vol 0 :capacity 7}]) path))))
  (testing "should return falsey when the target volume is not the end state of the path"
    (let [path [{:move :fill :glass 2} {:move :fill :glass 1}  {:move :empty :glass 1}]]
      (is (not ((has-target-vol? 5 [{:vol 0 :capacity 2}
                                                         {:vol 0 :capacity 4}
                                                         {:vol 0 :capacity 7}]) path))))))

(deftest check-solutions
  (testing "should return solution as fill glass 1 for glasses 0 - capacity 1, 1 - capacity
            2 for a target vol of 2"
    (is (= [{:move :fill :glass 1}] (first (take 1 (solutions [1 2] 2))))))
  (testing "should return solution as fill glass 2 for glasses 0 - capacity 2, 1 - capacity
            4, 2 - capacity 7 for a target vol of 5"
    (is (= [{:move :fill :glass 2} {:move :pour :from 2 :to 0}] (first (take 1 (solutions [2 4 7] 5)))))))
