(ns pipeit.eval
  (:require [pipeit.parse :as p])
  (:use [pipeit.utils])
  (:require [clojure.core.match :refer [match]]))

(defrecord Frame [parent-frame env])

(def root-frame
  (->Frame nil (hash-map)))

(defn new-frame [frame]
  (->Frame frame (hash-map)))

(defn update-env [frame name val]
  (assoc-in frame [:env name] val))

(defn lookup-env [frame name]
  (or ((:env frame) name)
      (lookup-env (:parent-frame frame) name)))

(declare eval-expr)

(defn eval-let-expr [frame {assignments :assignments, body-expr :body-expr}]
  (delay (let [updated-frame (reduce (fn [frame assignment]
                                       (update-env frame (:name assignment) (eval-expr frame (:expr assignment))))
                                     (new-frame frame)
                                     assignments)]
           (force (eval-expr updated-frame body-expr)))))

(defn eval-seq-expr [frame {node-type :node-type, exprs :exprs}]
  (delay (->> exprs
              (map (partial eval-expr frame))
              (apply (case node-type
                       :tuple-expr vector
                       :list-expr list
                       :set-expr set)))))

(defn eval-if-expr [frame {pred-expr :pred-expr, conseq-expr :conseq-expr, alt-expr :alt-expr}]
  (delay (if (force (eval-expr frame pred-expr))
           (eval-expr frame conseq-expr)
           (eval-expr frame alt-expr))))

(defn eval-when-expr [frame {pred-expr :pred-expr, conseq-expr :conseq-expr}]
  (delay (when (force (eval-expr frame pred-expr))
           (eval-expr frame conseq-expr))))

(defn eval-fun-expr [frame {params :params, body-expr :body-expr :as ast-node}]
  (if (empty? params)
    (eval-expr frame body-expr)                             ; a function without arguments is just a value
    (fn [x]
      (-> frame
          (update-env (first params) x)
          (eval-fun-expr (assoc ast-node :params (rest params)))
          force))))

(defn eval-application-expr [frame {fun-expr :fun-expr, arg-expr :arg-expr}]
  (delay ((force (eval-expr frame fun-expr)) (force (eval-expr frame arg-expr)))))

(defn eval-unary-expr [frame {op :op, expr :expr}]
  (delay (let [f (case op
                   "-" -
                   "+" +
                   "not" not
                   )]
           (f (force (eval-expr frame expr))))))

(defn eval-binary-expr [frame {op :op, lhs-expr :lhs-expr, rhs-expr :rhs-expr}]
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
    (delay (f (force (eval-expr frame lhs-expr))
              (force (eval-expr frame rhs-expr))))))

(defn eval-expr [frame {node-type :node-type :as ast-node}]
  (case node-type
    :let-expr (eval-let-expr frame ast-node)
    :if-expr (eval-if-expr frame ast-node)
    :when-expr (eval-when-expr frame ast-node)
    :fun-expr (eval-fun-expr frame ast-node)
    :application-expr (eval-application-expr frame ast-node)
    '(:tuple-expr :list-expr :set-expr) (eval-seq-expr frame ast-node)
    :unary-expr (eval-unary-expr frame ast-node)
    :binary-expr (eval-binary-expr frame ast-node)
    :literal-expr (:value ast-node)
    :identifier-expr (lookup-env frame (:value ast-node))))

(defn eval-string [s]
  (->> s
       p/parse-string
       (map (comp force (partial eval-expr root-frame)))
       (run! println)))
