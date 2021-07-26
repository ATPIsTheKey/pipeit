(ns pipeit.interpret.runtime.bool
  (:require [pipeit.interpret.runtime.base-types :as base-types])
  (:require [pipeit.interpret.runtime.base-protocols :as base-protocols])
  (:require [pipeit.interpret.runtime.base-methods :as base-methods]))

(defrecord Bool [type-descriptor val])

(defn make-bool [val]
  (->Bool (base-types/->TypeDescriptor ::base-types/bool [::base-protocols/equatable ::base-protocols/logical])
          val))

(def false*
  (make-bool false))

(def true*
  (make-bool true))

(derive ::base-types/bool ::bool-convertible)
(derive ::base-types/nil ::bool-convertible)
(derive ::base-types/int ::bool-convertible)
(derive ::base-types/real ::bool-convertible)
(derive ::base-types/list ::bool-convertible)
(derive ::base-types/tuple ::bool-convertible)
(derive ::base-types/int ::bool-convertible)

(defmethod base-methods/and [::bool-convertible ::bool-convertible]
  [{lhs :val} {rhs :val}]
  (make-bool (and (boolean lhs) (boolean rhs))))

(defmethod base-methods/or [::bool-convertible ::bool-convertible]
  [{lhs :val} {rhs :val}]
  (make-bool (or (boolean lhs) (boolean rhs))))

(defmethod base-methods/not [::bool-convertible ::bool-convertible]
  [{val :val}]
  (make-bool (not (boolean val))))

(defmethod base-methods/to-bool [::bool-convertible]
  [{val :val}]
  (make-bool (boolean val)))
