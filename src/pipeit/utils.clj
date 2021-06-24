(ns pipeit.utils
  (:require [clojure.data.json :as json]))

(declare _)

(defn in?
  "true if coll contains item."
  [coll item]
  (some (partial = item) coll))

(defn hash-map-to-json [the-map]
  (json/write-str the-map))

(defn bind-to [a f]
  (f a))

(defn map-keys [f m]
  (into (empty m) (for [[k v] m] [(f k) v])))

(defn map-vals [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))
