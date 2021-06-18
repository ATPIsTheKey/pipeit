(ns pipeit.eval
  (:require [pipeit.parse :as p])
  (:use [pipeit.utils])
  (:require [clojure.core.match :refer [match]]))

(defrecord Frame [parent-frame env])

(def root-frame
  (->Frame nil {}))

(defn new-frame [frame]
  (->Frame frame {}))

(defn update-env [frame name val]
  (assoc-in frame [:env name] val))

(defn lookup-env [frame name]
  (or ((:env frame) name)
      (lookup-env (:parent-frame frame) name)))

(declare eval-let-expr
         eval-seq-expr)

(defn eval [frame ast-node]
  (match [ast-node]
         [{:node-type   :let-expr,
           :rec?        _,                                  ; todo
           :assignments assignments,
           :body-expr   body-expr}] (eval-let-expr frame assignments body-expr)))

(defn eval-let-expr [frame assignments body-expr]
  (delay (eval (reduce (fn [frame assignment]
                         (update-env frame (:name assignment) (:expr assignment)))
                       (new-frame frame)
                       assignments)
               body-expr)))

(defn eval-seq-expr [frame exprs]
  (map (partial eval frame) exprs))
