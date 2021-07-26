(ns pipeit.pattern-match
  (:use [clojure.core.match :only [match]])
  (:require [pipeit.ast :as ast]))

(defprotocol CompilablePattern)


