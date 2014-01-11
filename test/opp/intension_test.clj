(ns opp.intension-test
  (:refer-clojure :exclude [comp complement constantly fn juxt partial])
  (:require [clojure.core :as core])
  (:use [clojure.test]
        [opp.intension]))

(deftest fn-test
  (testing "callable"
    (is (instance? Callable (fn [])))
    (is (= 42 (.call (fn [] 42)))))

  (testing "runnable"
    (is (instance? Runnable (fn [])))
    (is (nil? (.run (fn [] 42)))))

  (testing "fn arity"
    (let [f (fn [])]
      (is (thrown? clojure.lang.ArityException
                   (f nil)))))

  (testing "fn invocation"
    (let [f (fn [& xs] xs)]
      (dotimes [n 42]
        (let [args (not-empty (range n))
              sexp (list* f args)]
          (is (= (eval sexp) args))))))

  (testing "fn application"
    (let [f (fn [& xs] xs)
          args (range 42)]
      (is (= (apply f args) args))))

  (testing "fn equality"
    (is (= (fn [x] x) (fn [x] x)))
    (is (not= (fn [x] x) (fn [y] y)))
    (let [a 1 b 2]
      (is (= (fn [] ( + a b))
             (fn [] ( + a b))))
      (is (not= (fn [] ( + a b))
                (fn [] ( + 1 2))))))

  (testing "metadata"
    (is (= (meta (core/fn []))
           (meta (fn []))))
    (let [m {:a 1}
          f1 (fn [])
          f2 (with-meta f1 m)]
      (is (= f1 f2))
      (is (not (identical? f1 f2)))
      (is (nil? (meta f1)))
      (is (= m (meta f2))))))
