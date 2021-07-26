(ns pipeit.interpret.interpret
  (:refer-clojure :exclude [eval])
  (:require [pipeit.parse :as parse])
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
  (:use [clojure.tools.trace :only [trace]]))

(defprotocol Evaluable
  (eval [node frame]))

(defprotocol Executable
  (exec [node frame]))

(defrecord Frame [parent env])

(def root-frame
  (->Frame nil (hash-map)))

(defn new-frame [frame]
  (->Frame frame (hash-map)))

(defn update-env [frame name val]
  (assoc-in (force frame) [:env name] val))

(defn lookup-env [frame name]
  (or ((:env (force frame)) name)
      (when-let [parent-frame (:parent frame)]
        (lookup-env parent-frame name))))

(defn update-env-with-declarations [frame name expr]
  (update-env frame name (eval expr (delay (update-env-with-declarations frame name expr)))))

(extend-type LetExpr
  Evaluable
  (eval [{declarations :declarations, body-expr :body-expr} frame]
    (delay (force (eval body-expr
                        (reduce-kv (fn [frame init-name init-expr]
                                     (update-env-with-declarations frame init-name init-expr))
                                   (new-frame frame)
                                   declarations)))))
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type SeqExpr
  Evaluable
  (eval [frame {node-type :node-type, exprs :exprs}]
    (delay (->> exprs
                (map #(eval % frame))
                (apply (case node-type
                         :tuple-expr vector
                         :list-expr list
                         :set-expr hash-set)))))
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type IfExpr
  Evaluable
  (eval [{pred-expr :pred-expr, conseq-expr :conseq-expr, alt-expr :alt-expr} frame]
    (delay (if (force (eval pred-expr frame))
             (force (eval conseq-expr frame))
             (when alt-expr
               (force (eval alt-expr frame))))))
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type FunExpr
  Evaluable
  (eval [{params :params, body-expr :body-expr :as node} frame]
    (if (empty? params)
      (eval body-expr frame)                                ; a function without arguments is just a value
      (fn fun [x]
        (-> frame
            (update-env "@recur" fun)
            (update-env (first params) x)
            (as-> frame' (eval (assoc node :params (rest params)) frame'))
            force))))
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type ApplicationExpr
  Evaluable
  (eval [{fun-expr :fun-expr, arg-expr :arg-expr} frame]
    (delay ((force (eval fun-expr frame)) (eval arg-expr frame)))) ; do not force evaluation of argument as it may not be used in the function body
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type UnaryExpr
  Evaluable
  (eval [{op :op, expr :expr} frame]
    (delay (let [f (case op
                     "-" -
                     "+" +
                     "not" not)]
             (f (force (eval expr frame))))))
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type BinaryExpr
  Evaluable
  (eval [{op :op, lhs-expr :lhs-expr, rhs-expr :rhs-expr} frame]
    (let [f (case op
              "or" #(or %1 %2)
              "and" #(and %1 %2)
              "==" =
              "not=" not=
              ">" >
              "<" <
              "<=" <=
              ">=" >=
              "*" *
              "%" mod
              "//" quot
              "+" +
              "-" -
              "**" #(Math/pow %1 %2))]
      (delay (f (force (eval lhs-expr frame))
                (force (eval rhs-expr frame))))))
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type AtomExpr
  Evaluable
  (eval [{type :type, val :val} frame]
    (case type
      :literal val
      :ident (lookup-env frame val)))
  Executable
  (exec [node frame]
    [(eval node frame) frame]))

(extend-type DefStmt
  Executable
  (exec [{declarations :declarations} frame]
    (let [updated-frame (reduce-kv (fn [frame init-name init-expr]
                                     (update-env frame init-name (eval init-expr frame)))
                                   frame
                                   declarations)]
      [nil updated-frame])))

(extend-type Program
  Executable
  (exec [{stmts :stmts} frame]
    (reductions (fn [[_ frame] stmt]
                  (exec stmt frame))
                [nil frame]
                stmts)))

(defn run [s]
  (-> s
      parse/parse-string
      (exec root-frame)
      (as-> res (map (comp force first) res))
      rest
      vec))
