(ns pipeit.utils
  (:require [clojure.data.json :as json])
  (:require [clojure.set :as set]))

(declare _)

(defn in?
  "true if coll contains item."
  [coll item]
  (some (partial = item) coll))

(def hash-map->json
  json/write-str)

(defn on [a f]
  (f a))

(defn map-keys [f coll]
  (->> coll
       (map (fn [[k v]] [(f k) v]))
       (into (empty coll))))

(defn map-vals [f coll]
  (->> coll
       (map (fn [[k v]] [k (f v)]))
       (into (empty coll))))

(defmacro when-let*
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))

(defn symmetric-difference [a b]
  (set/union (set/difference a b)
                     (set/difference b a)))

(defn fix [f]
  (let [forced-f #(apply f (force %) %&)]
    #(apply forced-f (delay (fix f)) %&)))
