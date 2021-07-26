(ns pipeit.interpret.runtime.real
  (:require [pipeit.interpret.runtime.base-types :as base-types])
  (:require [pipeit.interpret.runtime.base-protocols :as base-protocols])
  (:require [pipeit.interpret.runtime.base-methods :as base-methods])
  (:require [pipeit.interpret.runtime.bool :as bool]))

(defrecord Real [type-descriptor val])

(defn make-real [val]
  (->Real (base-types/->TypeDescriptor ::base-types/real
                                       [::base-protocols/additive ::base-protocols/multiplicative
                                        ::base-protocols/equatable ::base-protocols/comparable])
          val))

(derive ::base-types/real ::real-promotable)
(derive ::base-types/int ::real-promotable)

(derive ::real-promotable ::real-convertible)

(defmethod base-methods/add [::base-types/real ::real-promotable]
  [{lhs-val :val} {rhs-val :val}]
  (make-real (+ lhs-val rhs-val)))

(defmethod base-methods/add [::real-promotable ::base-types/real]
  [{lhs-val :val} {rhs-val :val}]
  (make-real (+ lhs-val rhs-val)))

(defmethod base-methods/sub [::base-types/real ::real-promotable]
  [{lhs-val :val} {rhs-val :val}]
  (make-real (- lhs-val rhs-val)))

(defmethod base-methods/sub [::real-promotable ::base-types/real]
  [{lhs-val :val} {rhs-val :val}]
  (make-real (- lhs-val rhs-val)))

(defmethod base-methods/mul [::base-types/real ::real-promotable]
  [{lhs-val :val} {rhs-val :val}]
  (make-real (* lhs-val rhs-val)))

(defmethod base-methods/mul [::real-promotable ::base-types/real]
  [{lhs-val :val} {rhs-val :val}]
  (make-real (* lhs-val rhs-val)))

(defmethod base-methods/div [::real-promotable ::real-promotable]
  [{lhs-val :val} {rhs-val :val}]
  (make-real (double (/ lhs-val rhs-val))))

(defmethod base-methods/equal [::base-types/real ::base-types/real]
  [{lhs-val :val} {rhs-val :val}]
  (bool/make-bool (= lhs-val rhs-val)))

(defmethod base-methods/not-equal [::base-types/real ::base-types/real]
  [{lhs-val :val} {rhs-val :val}]
  (bool/make-bool (not= lhs-val rhs-val)))

(defmethod base-methods/to-real [::real-promotable]
  [{val :val}]
  (make-real (double val)))
