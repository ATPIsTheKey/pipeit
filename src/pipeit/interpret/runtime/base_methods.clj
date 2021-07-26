(ns pipeit.interpret.runtime.base-methods
  (:refer-clojure :exclude [int list apply first rest map reduce and or not hash])
  (:require [pipeit.interpret.runtime.base-types :as base-types]))

(defmulti repr base-types/type-dispatcher)

(defmulti to-bool base-types/type-dispatcher)
(defmulti to-int base-types/type-dispatcher)
(defmulti to-real base-types/type-dispatcher)
(defmulti to-list base-types/type-dispatcher)
(defmulti to-tuple base-types/type-dispatcher)

(defmulti add base-types/type-dispatcher)
(defmulti sub base-types/type-dispatcher)
(defmulti mul base-types/type-dispatcher)
(defmulti div base-types/type-dispatcher)

(defmulti and base-types/type-dispatcher)
(defmulti or base-types/type-dispatcher)
(defmulti not base-types/type-dispatcher)

(defmulti equal base-types/type-dispatcher)
(defmulti not-equal base-types/type-dispatcher)

(defmulti greater base-types/type-dispatcher)
(defmulti greater-equal base-types/type-dispatcher)
(defmulti less base-types/type-dispatcher)
(defmulti less-equal base-types/type-dispatcher)

(defmulti apply base-types/type-dispatcher)

(defmulti first base-types/type-dispatcher)
(defmulti rest base-types/type-dispatcher)
(defmulti len base-types/type-dispatcher)

(defmulti conjoin base-types/type-dispatcher)
(defmulti disjoin base-types/type-dispatcher)

(defmulti map base-types/type-dispatcher)
(defmulti reduce base-types/type-dispatcher)

(defmulti hash base-types/type-dispatcher)
