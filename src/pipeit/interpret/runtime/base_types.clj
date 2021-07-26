(ns pipeit.interpret.runtime.base-types)

(defrecord TypeDescriptor [type protocol-impls])

(keyword (str *ns*) "nil")
(keyword (str *ns*) "bool")
(keyword (str *ns*) "int")
(keyword (str *ns*) "real")
(keyword (str *ns*) "string")
(keyword (str *ns*) "list")
(keyword (str *ns*) "tuple")
(keyword (str *ns*) "map")
(keyword (str *ns*) "function")

(derive ::any ::nil)
(derive ::any ::bool)
(derive ::any ::int)
(derive ::any ::real)
(derive ::any ::string)
(derive ::any ::list)
(derive ::any ::tuple)
(derive ::any ::map)
(derive ::any ::function)

(defn type-dispatcher [& args]
  (mapv (comp :type :type-descriptor) args))

(defrecord Type [name protocols val])
