(ns pipeit.eval-test
  (:require [clojure.test :refer :all])
  (:use [pipeit.interpret.eval]))

(deftest let-expr
  (is (= (eval-prog "let a = 2 in a;") (list 2)))
  (is (= (eval-prog "let a = 2 in let b = 3 in a + b;") (list 5)))
  (is (= (eval-prog "let a = 2, b = a + 3 in b;") (list 5)))
  (is (= (eval-prog "let a = 2 in let a = 3 in a;") (list 3))))

(deftest simple-fun
  (is (= (eval-prog "(|| -> 1);") (list 1)))
  (is (function? (first (eval-prog "|a| -> a;"))))
  (is (= (eval-prog "(|a| -> a) 1;") (list 1)))
  (is (= (eval-prog "(|a b c| -> a) 1 2 3;") (list 1)))
  (is (= (eval-prog "(|a| -> |b| -> |c| -> a) 1 2 3;") (list 1)))
  (is (function? (first (eval-prog "(|a b c| -> a) 1;"))))
  (is (function? (first (eval-prog "(|a| -> |b| -> |c| -> a) 1;"))))
  (is (function? (first (eval-prog "((|a b c| -> a) 1) 2;"))))
  (is (function? (first (eval-prog "((|a| -> |b| -> |c| -> a) 1) 2;"))))
  (is (= (eval-prog "(((|a b c| -> a) 1) 2) 3;") (list 1)))
  (is (= (eval-prog "(((|a| -> |b| -> |c| -> a) 1) 2) 3;") (list 1)))
  (is (= (eval-prog "let Plus2 = (|a b| -> a + b) 2 in Plus2 4;") (list 6)))
  (is (= (eval-prog "let Z = |f| -> (|x| -> f (|v| -> x x v)) (|x| -> f (|v| -> x x v)),
                           T = |this n| -> 1 if n == 0 else this (n - 1) * n,
                           Fact = Z T in Fact 0;") (list 1)))
  (is (= (eval-prog "let Z = |f| -> (|x| -> f (|v| -> x x v)) (|x| -> f (|v| -> x x v)),
                           T = |this n| -> 1 if n == 0 else this (n - 1) * n,
                           Fact = Z T in Fact 1;") (list 1)))
  (is (= (eval-prog "let Z = |f| -> (|x| -> f (|v| -> x x v)) (|x| -> f (|v| -> x x v)),
                           T = |this n| -> 1 if n == 0 else this (n - 1) * n,
                           Fact = Z T in Fact 3;") (list 6))))

(deftest laziness
  (is (= (eval-prog "let Y = |f| -> (|x| -> f (x x)) (|x| -> f (x x)),
                           T = |this n| -> 1 if n == 0 else this (n - 1) * n,
                           Fact = Y T in Fact 0;") (list 1)))
  (is (= (eval-prog "let Y = |f| -> (|x| -> f (x x)) (|x| -> f (x x)),
                           T = |this n| -> 1 if n == 0 else this (n - 1) * n,
                           Fact = Y T in Fact 1;") (list 1)))
  (is (= (eval-prog "let Y = |f| -> (|x| -> f (x x)) (|x| -> f (x x)),
                           T = |this n| -> 1 if n == 0 else this (n - 1) * n,
                           Fact = Y T in Fact 3;") (list 6))))

(deftest recursion)
