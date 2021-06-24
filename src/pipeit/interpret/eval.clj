(ns pipeit.interpret.eval
  (:require [pipeit.parse :as p])
  (:use [pipeit.utils])
  (:require [clojure.core.match :refer [match]])
  (:use [clojure.tools.trace]))

(defrecord Frame [parent env])

(def root-frame
  (->Frame nil (hash-map)))

(defn new-frame [frame]
  (->Frame frame (hash-map)))

(defn update-env [frame name val]
  (assoc-in frame [:env name] val))

(defn lookup-env [frame name]
  (or ((:env frame) name)
      (when-let [parent-frame (:parent frame)]
        (lookup-env parent-frame name))))

(declare eval-expr)

(defn eval-let-expr [frame {assignments :assignments, body-expr :body-expr}]
  (delay (let [updated-frame (reduce-kv (fn [frame init-name init-expr]
                                          (update-env frame init-name (eval-expr frame init-expr)))
                                        (new-frame frame)
                                        assignments)]
           (force (eval-expr updated-frame body-expr)))))

(defn eval-seq-expr [frame {node-type :node-type, exprs :exprs}]
  (delay (->> exprs
              (map (partial eval-expr frame))
              (apply (case node-type
                       :tuple-expr vector
                       :list-expr list
                       :set-expr hash-set)))))

(defn eval-if-expr [frame {pred-expr :pred-expr, conseq-expr :conseq-expr, alt-expr :alt-expr}]
  (delay (if (force (eval-expr frame pred-expr))
           (force (eval-expr frame conseq-expr))
           (force (eval-expr frame alt-expr)))))

(defn eval-when-expr [frame {pred-expr :pred-expr, conseq-expr :conseq-expr}]
  (delay (when (force (eval-expr frame pred-expr))
           (force (eval-expr frame conseq-expr)))))

(defn eval-fun-expr [frame {params :params, body-expr :body-expr :as ast-node}]
  (if (empty? params)
    (eval-expr frame body-expr)                             ; a function without arguments is just a value
    (fn [x]
      (-> frame
          (update-env (first params) x)
          (eval-fun-expr (assoc ast-node :params (rest params)))
          force))))

(defn eval-application-expr [frame {fun-expr :fun-expr, arg-expr :arg-expr}]
  (delay ((force (eval-expr frame fun-expr)) (eval-expr frame arg-expr)))) ; do not force evaluation of argument as it may not be used in the function body

(defn eval-unary-expr [frame {op :op, expr :expr}]
  (delay (let [f (case op
                   "-" -
                   "+" +
                   "not" not)]
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
  "Evaluates a pipeit expression. An expression does have no effects on the top frame it is evaluated with.
  (frame stmt) -> object"
  (case node-type
    :let-expr (eval-let-expr frame ast-node)
    :if-expr (eval-if-expr frame ast-node)
    :when-expr (eval-when-expr frame ast-node)
    :fun-expr (eval-fun-expr frame ast-node)
    :application-expr (eval-application-expr frame ast-node)
    (:tuple-expr :list-expr :set-expr) (eval-seq-expr frame ast-node)
    :unary-expr (eval-unary-expr frame ast-node)
    :binary-expr (eval-binary-expr frame ast-node)
    :literal-expr (:value ast-node)
    :identifier-expr (force (lookup-env frame (:value ast-node)))))

(defn eval-def-stmt [frame {assignments :assignments}]
  (let [updated-frame (reduce-kv (fn [frame init-name init-expr]
                                   (update-env frame init-name (eval-expr frame init-expr)))
                                 frame
                                 assignments)]
    [updated-frame nil]))

(defn eval-stmt [frame {node-type :node-type :as ast-node}]
  "Evaluates a pipeit statement. A statement does have effects on the top frame it is evaluated with.
  (frame stmt) -> (frame ?object)"
  (case node-type
    :def-stmt (eval-def-stmt frame ast-node)
    [frame (eval-expr frame ast-node)]))                    ; evaluating an expression has no effects on the frame

(defn eval-prog [s]
  (->> s
       p/parse-string
       (reductions (fn [[frame _] stmt]
                     (eval-stmt frame stmt))
                   [root-frame nil])
       (map (fn [[_ obj]] (force obj)))
       rest))
