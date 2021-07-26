(ns pipeit.interpret_test.interpret_test
  (:require [clojure.test :refer :all])
  (:use [pipeit.interpret.interpret :only [run]]))

(deftest let-expr
  (is (= (run "let a = 2 in a;") [2]))
  (is (= (run "let a = 2 in let b = 3 in a + b;") [5]))
  (is (= (run "let a = 2, b = a + 3 in b;") [5]))
  (is (= (run "let a = 2 in let a = 3 in a;") [3])))

(deftest non-rec-funs
  (is (= (run "(|| -> 1);") [1]))
  (is (every? function? (run "|a| -> a;")))
  (is (= (run "(|a| -> a) 1;") [1]))
  (is (= (run "(|a b c| -> a) 1 2 3;") [1]))
  (is (= (run "(|a| -> |b| -> |c| -> a) 1 2 3;") [1]))
  (is (every? function? (run "(|a b c| -> a) 1;")))
  (is (every? function? (run "(|a| -> |b| -> |c| -> a) 1;")))
  (is (every? function? (run "((|a b c| -> a) 1) 2;")))
  (is (every? function? (run "((|a| -> |b| -> |c| -> a) 1) 2;")))
  (is (= (run "(((|a b c| -> a) 1) 2) 3;") [1]))
  (is (= (run "(((|a| -> |b| -> |c| -> a) 1) 2) 3;") [1]))
  (is (= (run "let Plus2 = (|a b| -> a + b) 2 in Plus2 4;") [6]))
  (is (= (run "let Z = |f| -> (|x| -> f (|v| -> x x v)) (|x| -> f (|v| -> x x v)),
                           T = |this n| -> if n == 0 then 1 else this (n - 1) * n,
                           Fact = Z T in Fact 0;") [1]))
  (is (= (run "let Z = |f| -> (|x| -> f (|v| -> x x v)) (|x| -> f (|v| -> x x v)),
                           T = |this n| -> if n == 0 then 1 else this (n - 1) * n,
                           Fact = Z T in Fact 1;") [1]))
  (is (= (run "let Z = |f| -> (|x| -> f (|v| -> x x v)) (|x| -> f (|v| -> x x v)),
                           T = |this n| -> if n == 0 then 1 else this (n - 1) * n,
                           Fact = Z T in Fact 3;") [6])))

(deftest rec-funs
  (is (= (run "let fact = |n| -> if n == 1 then 1 else n * fact (n - 1) in fact 1;") [1]))
  (is (= (run "let fact = |n| -> if n == 1 then 1 else n * fact (n - 1) in fact 3;") [6])))

(deftest laziness
  (is (= (run "let Y = |f| -> (|x| -> f (x x)) (|x| -> f (x x)),
                           T = |this n| -> if n == 0 then 1 else this (n - 1) * n,
                           Fact = Y T in Fact 0;") [1]))
  (is (= (run "let Y = |f| -> (|x| -> f (x x)) (|x| -> f (x x)),
                           T = |this n| -> if n == 0 then 1 else this (n - 1) * n,
                           Fact = Y T in Fact 1;") [1]))
  (is (= (run "let Y = |f| -> (|x| -> f (x x)) (|x| -> f (x x)),
                           T = |this n| -> if n == 0 then 1 else this (n - 1) * n,
                           Fact = Y T in Fact 3;") [6])))

"let fact = |n| -> if n == 1 then 1 else n * fact (n - 1) in fact 1;"
