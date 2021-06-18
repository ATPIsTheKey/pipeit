(defproject pipeit "0.1.0-SNAPSHOT"
  :description "A minimalisitc, pure functional programming language that encourages peipeline style programming."
  :url "https://github.com/ATPIsTheKey/pipeit"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/data.json "2.3.1"]
                 [org.blancas/kern "1.1.0"]]
  :repl-options {:init-ns pipeit.core})
