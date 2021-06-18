(ns pipeit.utils
  (:require [clojure.data.json :as json]))

(declare _)

(defn in?
  "true if coll contains item."
  [coll item]
  (some (partial = item) coll))

(defn map-to-json [the-map]
  (json/write-str the-map))

(defn bind-to [a f]
  (f a))

