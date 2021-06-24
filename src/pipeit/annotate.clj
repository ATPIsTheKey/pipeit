(ns pipeit.annotate
  (:use [pipeit.utils])
  (:require [clojure.set :as set])
  (:use [clojure.core.match]))

(defn annotate-recursion-points [ast-node]
  (match [ast-node]
         [{}]))

(defn assoc-variable-types [ast-node & {:keys [vars free-vars bound-vars] :or {vars       #{},
                                                                               free-vars  #{},
                                                                               bound-vars #{}}}]
  (merge ast-node {:vars       vars,
                   :free-vars  free-vars,
                   :bound-vars bound-vars}))

(defn annotate-variable-types [ast-node]
  (case (:node-type ast-node)
    :identifier-expr (assoc-variable-types ast-node
                                           :vars (hash-set val))

    (:tuple-expr :list-expr :set-expr) (update ast-node :exprs (partial map annotate-variable-types))

    :let-expr (as-> ast-node node
                    (update node :body-expr annotate-variable-types)
                    (update node :assignments (partial map-vals annotate-variable-types))
                    (assoc-variable-types node
                                          :vars (apply set/union
                                                       (->> node
                                                            :body-expr
                                                            :vars)
                                                       (->> node
                                                            :assignments
                                                            vals
                                                            (map :vars)))) ; a list of sets
                    (assoc-variable-types :free-vars (apply set/difference
                                                            (:vars node)
                                                            (->> node
                                                                 :assignments
                                                                 keys
                                                                 set))))

    :unary-expr (as-> ast-node node
                      (update node :expr annotate-variable-types)
                      (assoc-variable-types node
                                            (select-keys (:expr node) [:vars :free-vars :bound-vars])))

    :binary-expr (as-> ast-node node
                       (update node :lhs-expr annotate-variable-types)
                       (update node :rhs-expr annotate-variable-types)
                       (assoc-variable-types node
                                             :vars (set/union (get-in node [:lhs-expr :vars])
                                                              (get-in node [:rhs-expr :vars]))
                                             :free-vars (set/union (get-in node [:lhs-expr :free-vars])
                                                                   (get-in node [:rhs-expr :free-vars]))
                                             :bound-vars (set/union (get-in node [:lhs-expr :bound-vars])
                                                                    (get-in node [:rhs-expr :bound-vars]))))
    :application-expr (as-> ast-node node
                            (update node :fun-expr annotate-variable-types)
                            (update node :arg-expr annotate-variable-types)
                            (assoc-variable-types node
                                                  :vars (set/union (get-in node [:lhs-expr :vars])
                                                                   (get-in node [:rhs-expr :vars]))
                                                  :free-vars (set/union (get-in node [:lhs-expr :free-vars])
                                                                        (get-in node [:rhs-expr :free-vars]))
                                                  :bound-vars (set/union (get-in node [:lhs-expr :bound-vars])
                                                                         (get-in node [:rhs-expr :bound-vars]))))
    (assoc-variable-types ast-node)))

