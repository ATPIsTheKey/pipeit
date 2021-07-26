(ns pipeit.interpret.runtime.base-protocols)

(keyword (str *ns*) "additive")
(keyword (str *ns*) "multiplicative")
(keyword (str *ns*) "equatable")
(keyword (str *ns*) "comparable")
(keyword (str *ns*) "logical")
(keyword (str *ns*) "sequentiable")
(keyword (str *ns*) "collective")
(keyword (str *ns*) "stringable")
(keyword (str *ns*) "mappable")
(keyword (str *ns*) "reducable")
(keyword (str *ns*) "enumerable")
(keyword (str *ns*) "hashable")

(defn protocol-dispatcher [& args]
  (mapv (comp :type :type-descriptor) args))
