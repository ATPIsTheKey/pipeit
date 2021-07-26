(ns pipeit.interpret.runtime.list
  (:refer-clojure :exclude [int])
  (:require [pipeit.interpret.runtime.base-types :as base-types])
  (:require [pipeit.interpret.runtime.base-protocols :as base-protocols])
  (:require [pipeit.interpret.runtime.base-methods :as base-methods])
  (:require [pipeit.interpret.runtime.nil :as anil])
  (:require [pipeit.interpret.runtime.bool :as bool])
  (:require [pipeit.interpret.runtime.int :as int])
  (:import (clojure.lang Seqable)))

(defrecord List [type-descriptor head tail len])

(defn make-list [first rest len]
  (->List (base-types/->TypeDescriptor ::base-types/list [:sequentiable :collective :mappable :reducible :equatable])
          first
          rest
          len))

(def empty-list
  (make-list anil/nil* anil/nil* 0))

(defmethod base-methods/first [::base-types/list]
  [ls]
  (:first ls))

(defmethod base-methods/rest [::base-types/rest]
  [{rest :rest}]
  (if (anil/nil*? rest)
    empty-list
    rest))

(defmethod base-methods/len [::base-types/list]
  [ls]
  (int/make-int (:len ls)))

(defmethod base-methods/map [::base-types/function ::base-types/list]
  [f {len :len :as ls}]
  (make-list (f (base-methods/first ls))
             (base-methods/map f (base-methods/rest ls))
             len))

(defmethod base-methods/reduce [::base-types/function ::base-types/any ::base-types/list]
  [f init {first :first :as ls}]
  (if (nil? first)                                          ; todo: implement nil type
    init
    (recur f
           (f init (base-methods/first ls))
           (base-methods/rest ls))))

(defmethod base-methods/conjoin [::base-types/list ::base-types/any]
  [{len :len :as ls} item]
  (make-list item ls (inc len)))

(defmethod base-methods/disjoin [::base-types/list ::base-types/any]
  [{len :len :as ls} item]
  (if (base-methods/equal (base-methods/first ls) item)
    (base-methods/rest ls)
    (make-list (base-methods/first ls)
               (base-methods/disjoin (base-methods/rest ls) item)
               (dec len))))
