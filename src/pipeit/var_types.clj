(ns pipeit.var-types
  (:import (pipeit.ast AtomExpr
                       SeqExpr
                       UnaryExpr
                       BinaryExpr
                       FunExpr
                       ApplicationExpr
                       IfExpr
                       LetExpr
                       DefStmt
                       Program))
  (:use [pipeit.utils])
  (:require [clojure.set :as set])
  (:use [clojure.tools.trace :only [trace]]))

(defprotocol VarTypes
  (-vars [node])
  (-free-vars [node])
  (-bound-vars [node]))

(declare vars free-vars bound-vars)

(extend-type AtomExpr
  VarTypes
  (-vars [{type :type, val :val}]
    (if (= type :ident)
      #{val}
      #{}))
  (-free-vars [node]
    (vars node))
  (-bound-vars [_]
    hash-set))

(extend-type SeqExpr
  VarTypes
  (-vars [{exprs :exprs}]
    (apply set/union (map vars exprs)))
  (-free-vars [{exprs :exprs}]
    (apply set/union (map free-vars exprs)))
  (-bound-vars [node]
    (set/difference (vars node)
                    (free-vars node))))

(extend-type UnaryExpr
  VarTypes
  (-vars [{expr :expr}]
    (vars expr))
  (-free-vars [{expr :expr}]
    (free-vars expr))
  (-bound-vars [{expr :expr}]
    (bound-vars expr)))

(extend-type BinaryExpr
  VarTypes
  (-vars [{lhs-expr :lhs-expr, rhs-expr :rhs-expr}]
    (set/union (vars lhs-expr)
               (vars rhs-expr)))
  (-free-vars [{lhs-expr :lhs-expr, rhs-expr :rhs-expr}]
    (set/union (free-vars lhs-expr)
               (free-vars rhs-expr)))
  (-bound-vars [node]
    (set/difference (vars node)
                    (free-vars node))))

(extend-type FunExpr
  VarTypes
  (-vars [{body-expr :body-expr}]
    (vars body-expr))
  (-free-vars [{params :params, body-expr :body-expr}]
    (apply disj (free-vars body-expr) params))
  (-bound-vars [node]
    (set/difference (vars node)
                    (free-vars node))))

(extend-type ApplicationExpr
  VarTypes
  (-vars [{fun-expr :fun-expr, arg-expr :arg-expr}]
    (set/union (vars fun-expr)
               (vars arg-expr)))
  (-free-vars [{fun-expr :fun-expr, arg-expr :arg-expr}]
    (set/union (free-vars fun-expr)
               (free-vars arg-expr)))
  (-bound-vars [node]
    (set/difference (vars node)
                    (free-vars node))))

(extend-type IfExpr
  VarTypes
  (-vars [{pred-expr :pred-expr, conseq-expr :conseq-expr, alt-expr :alt-expr}]
    (set/union (vars pred-expr)
               (vars conseq-expr)
               (vars alt-expr)))
  (-free-vars [{pred-expr :pred-expr, conseq-expr :conseq-expr, alt-expr :alt-expr}]
    (set/union (free-vars pred-expr)
               (free-vars conseq-expr)
               (free-vars alt-expr)))
  (-bound-vars [node]
    (set/difference (vars node)
                    (free-vars node))))

(extend-type LetExpr
  VarTypes
  (-vars [{declarations :declarations, body-expr :body-expr}]
    (set/union
      (apply set/union (map vars (vals declarations)))
      (vars body-expr)))
  (-free-vars [{declarations :declarations, body-expr :body-expr}]
    (set/union
      (set/difference (free-vars body-expr)
                      (set (keys declarations)))
      (apply set/union
             (map set/difference
                  (reductions set/union (map free-vars (vals declarations)))
                  (reductions set/union (map hash-set (keys declarations)))))))
  (-bound-vars [{declarations :declarations, body-expr :body-expr}]
    (set/union
      (set/intersection (free-vars body-expr)
                        (set (keys declarations)))
      (apply set/union
             (map set/intersection
                  (reductions set/union (map free-vars (vals declarations)))
                  (reductions set/union (map hash-set (keys declarations))))))))

(extend-type DefStmt
  VarTypes
  (-vars [{declarations :declarations}]
    (apply set/union (map vars (vals declarations))))
  (-free-vars [{declarations :declarations}]
    (apply set/union
           (map set/difference
                (reductions set/union (map free-vars (vals declarations)))
                (reductions set/union (map hash-set (keys declarations))))))
  (-bound-vars [{declarations :declarations}]
    (apply set/union
           (map set/intersection
                (reductions set/union (map free-vars (vals declarations)))
                (reductions set/union (map hash-set (keys declarations)))))))

(extend-type Program
  VarTypes
  (-vars [{stmts :stmts}]
    (apply set/union (map vars stmts)))
  (-free-vars [{stmts :stmts}]
    (apply set/union (map free-vars stmts)))
  (-bound-vars [{stmts :stmts}]
    (apply set/union (map bound-vars stmts))))

(def vars
  (memoize -vars))

(def free-vars
  (memoize -free-vars))

(def bound-vars
  (memoize -bound-vars))
