(ns pipeit.interpret.runtime.tuple
  (:require [pipeit.interpret.runtime.base-types :as base-types])
  (:require [pipeit.interpret.runtime.base-protocols :as base-protocols])
  (:require [pipeit.interpret.runtime.base-methods :as base-methods])
  (:require [pipeit.interpret.runtime.bool :as bool]))

(defrecord Tuple [type-descriptor items])

(defn make-tuple [& items]
  (->Tuple (base-types/->TypeDescriptor ::base-types/tuple [::base-protocols/sequentiable ::base-protocols/mappable
                                                            ::base-protocols/reducible ::base-protocols/equatable])
           (vec items)))

(defmethod base-methods/first [::base-types/tuple]
  [{val :val}]
  (first val))

(defmethod base-methods/rest [::base-types/tuple]
  [tup]
  (base-methods/rest (base-methods/to-list tup)))

(defmethod base-methods/len [::base-types/tuple]
  [{val :val}]
  (count val))

(defmethod base-methods/map [::base-types/tuple ::base-types/function]
  [f tup]
  (base-methods/map f (base-methods/to-list tup)))

(defmethod base-methods/reduce [::base-types/function ::any ::base-types/tuple]
  [f init tup]
  (base-methods/reduce f init (base-methods/to-list tup)))

(defmethod base-methods/equal [::base-types/tuple]
  [{lhs-val :val} {rhs-val :val}]
  (bool/make-bool (= lhs-val rhs-val)))

(defmethod base-methods/not-equal [::base-types/tuple]
  [{lhs-val :val} {rhs-val :val}]
  (bool/make-bool (= lhs-val rhs-val)))
