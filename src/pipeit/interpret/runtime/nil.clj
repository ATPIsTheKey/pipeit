(ns pipeit.interpret.runtime.nil
  (:refer-clojure :exclude [list])
  (:require [pipeit.interpret.runtime.base-types :as base-types])
  (:require [pipeit.interpret.runtime.base-protocols :as base-protocols])
  (:require [pipeit.interpret.runtime.base-methods :as base-methods])
  (:require [pipeit.interpret.runtime.bool :as bool])
  (:require [pipeit.interpret.runtime.int :as int])
  (:require [pipeit.interpret.runtime.list :as list]))

(defrecord Nil [type-descriptor])

(def nil*
  (->Nil (base-types/->TypeDescriptor ::base-types/bool [::base-protocols/sequentiable ::base-protocols/equatable])))

(def nil*?
  (partial = nil*))

(defmethod base-methods/first [::base-types/nil]
  [_]
  nil*)

(defmethod base-methods/rest [::base-types/nil]
  [_]
  list/empty-list)

(defmethod base-methods/len [::base-types/nil]
  [_]
  (int/make-int 0))

(defmethod base-methods/map [::base-types/function ::base-types/nil]
  [_ _]
  list/empty-list)

(defmethod base-methods/reduce [::base-types/function ::base-types/any ::base-types/nil]
  [_ init _]
  init)

(defmethod base-methods/conjoin [::base-types/list ::base-types/any]
  [_ item]
  (base-methods/conjoin list/empty-list item))

(defmethod base-methods/disjoin [::base-types/list ::base-types/any]
  [_ _]
  nil*)

(defmethod base-methods/equal [::base-types/nil ::base-types/nil]
  [_ _]
  bool/true*)

(defmethod base-methods/not-equal [::base-types/nil ::base-types/nil]
  [_ _]
  bool/false*)
