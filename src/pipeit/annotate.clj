(ns pipeit.annotate
  (:use [pipeit.utils])
  (:require [clojure.set :as set]))

(defn annotate-variable-types [ast-node]
  (case (:node-type ast-node)
    :program (update ast-node :stmts (partial map annotate-variable-types))

    :def-stmt (update :assignments ast-node (partial map-vals annotate-variable-types)) ; todo: ignores order of assignments

    :identifier-expr (as-> ast-node node
                           (assoc node :vars #{(:value node)})
                           (assoc node :free-vars (:vars node))
                           (assoc node :bound-vars #{}))

    (list :tuple-expr
          :list-expr
          :set-expr) (as-> ast-node node
                           (update node :exprs (partial map annotate-variable-types))
                           (assoc node :vars (apply set/union
                                                    (->> node :exprs (map :vars))))
                           (assoc node :free-vars (apply set/union
                                                         (->> node :exprs (map :free-vars))))
                           (assoc node :bound-vars (set/difference (:vars node)
                                                                   (:free-vars node))))

    :let-expr (as-> ast-node node
                    (update node :body-expr annotate-variable-types)
                    (update node :assignments (partial map-vals annotate-variable-types)) ; todo: ignores order of assignments
                    ;; the set of vars is the union of all sets of vars of body-expr and init-exprs
                    (assoc node :vars (apply set/union
                                             (->> node :body-expr :vars)
                                             (->> node :assignments vals (map :vars))))
                    ;; the set of free vars is the difference of the union of all sets in the init exprs and the body expr with the
                    ;; set of assignment names
                    (assoc node :free-vars (set/difference (apply set/union
                                                                  (->> node :body-expr :free-vars)
                                                                  (->> node :assignments vals (map :free-vars))) ; a seq of sets
                                                           (->> node :assignments keys set)))
                    ;; the set of bound vars is the difference of the set of all vars and the set of free vars
                    (assoc node :bound-vars (set/difference (:vars node)
                                                            (:free-vars node))))

    :unary-expr (as-> ast-node node
                      (update node :expr annotate-variable-types)
                      (assoc node :vars (->> node :expr :vars))
                      (assoc node :free-vars (->> node :expr :free-vars))
                      (assoc node :bound-vars (->> node :expr :bound-vars)))

    :binary-expr (as-> ast-node node
                       (update node :lhs-expr annotate-variable-types)
                       (update node :rhs-expr annotate-variable-types)
                       (assoc node :vars (set/union (->> node :lhs-expr :vars)
                                                    (->> node :rhs-expr :vars)))
                       (assoc node :free-vars (set/union (->> node :lhs-expr :free-vars)
                                                         (->> node :rhs-expr :free-vars)))
                       (assoc node :bound-vars (set/difference (:vars node)
                                                               (:free-vars node))))
    :application-expr (as-> ast-node node
                            (update node :fun-expr annotate-variable-types)
                            (update node :arg-expr annotate-variable-types)
                            (assoc node :vars (set/union (->> node :fun-expr :vars)
                                                         (->> node :arg-expr :vars)))
                            (assoc node :free-vars (set/union (->> node :fun-expr :free-vars)
                                                              (->> node :arg-expr :free-vars)))
                            (assoc node :bound-vars (set/difference (:vars node)
                                                                    (:free-vars node))))
    :fun-expr (as-> ast-node node
                    (update node :body-expr annotate-variable-types)
                    (assoc node :vars (->> node :body-expr :vars))
                    (assoc node :free-vars (set/difference (->> node :body-expr :free-vars)
                                                           (->> node :params set)))
                    (assoc node :bound-vars (set/difference (:vars node)
                                                            (:free-vars node))))

    :if-expr (as-> ast-node node
                   (update node :pred-expr annotate-variable-types)
                   (update node :conseq-expr annotate-variable-types)
                   (update node :alt-expr annotate-variable-types)
                   (assoc node :vars (set/union (->> node :pred-expr :vars)
                                                (->> node :conseq-expr :vars)
                                                (->> node :alt-expr :vars)))
                   (assoc node :free-vars (set/union (->> node :pred-expr :free-vars)
                                                     (->> node :conseq-expr :free-vars)
                                                     (->> node :alt-expr :free-vars)))
                   (assoc node :bound-vars (set/union (->> node :pred-expr :bound-vars)
                                                      (->> node :conseq-expr :bound-vars)
                                                      (->> node :alt-expr :bound-vars))))

    :when-expr (as-> ast-node node
                     (update node :pred-expr annotate-variable-types)
                     (update node :conseq-expr annotate-variable-types)
                     (assoc node :vars (set/union (->> node :pred-expr :vars)
                                                  (->> node :conseq-expr :vars)))
                     (assoc node :free-vars (set/union (->> node :pred-expr :free-vars)
                                                       (->> node :conseq-expr :free-vars)))
                     (assoc node :bound-vars (set/union (->> node :pred-expr :bound-vars)
                                                        (->> node :conseq-expr :bound-vars))))

    ;; default case
    (as-> ast-node node
          (assoc node :vars #{})
          (assoc node :free-vars #{})
          (assoc node :bound-vars #{}))))
