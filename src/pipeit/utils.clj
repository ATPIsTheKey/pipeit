(ns pipeit.utils
  (:require [clojure.data.json :as json]))

(declare _)

(defn in?
  "true if coll contains item."
  [coll item]
  (some (partial = item) coll))

(def hash-map-to-json
  json/write-str)

(defn bind-to [a f]
  (f a))

(defn map-keys [f coll]
  (->> coll (map (fn [[k v]] [(f k) v])) (into (empty coll))))

(defn map-vals [f coll]
  (->> coll (map (fn [[k v]] [k (f v)])) (into (empty coll))))
