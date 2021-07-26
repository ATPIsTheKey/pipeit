(ns pipeit.interpret.runtime.int
  (:require [pipeit.interpret.runtime.base-types :as base-types])
  (:require [pipeit.interpret.runtime.base-protocols :as base-protocols])
  (:require [pipeit.interpret.runtime.base-methods :as base-methods])
  (:require [pipeit.interpret.runtime.bool :as bool]))

(defrecord Int [type-descriptor val])

(defn make-int [val]
  (->Int (base-types/->TypeDescriptor ::base-types/int
                                      [::base-protocols/additive ::base-protocols/multiplicative
                                       ::base-protocols/equatable ::base-protocols/comparable])
         val))

(derive ::base-types/real ::int-convertible)

(defmethod base-methods/add [::base-types/int ::base-types/int]
  [{lhs :val} {rhs :val}]
  (make-int (+ lhs rhs)))

(defmethod base-methods/sub [::base-types/int ::base-types/int]
  [{lhs :val} {rhs :val}]
  (make-int (- lhs rhs)))

(defmethod base-methods/mul [::base-types/int ::base-types/int]
  [{lhs :val} {rhs :val}]
  (make-int (* lhs rhs)))

; todo div

(defmethod base-methods/equal [::base-types/int ::base-types/int]
  [{lhs :val} {rhs :val}]
  (bool/make-bool (= lhs rhs)))

(defmethod base-methods/not-equal [::base-types/int ::base-types/int]
  [{lhs :val} {rhs :val}]
  (bool/make-bool (not= lhs rhs)))

(defmethod base-methods/to-int [::int-convertible]
  [{val :val}]
  (make-int (int val)))
