(ns pipeit.parse.combinators
  (:use [blancas.kern.core]))

(defn <|:>
  ([p q] (<|> (<:> p) (<:> q)))
  ([p q & more] (reduce <|:> (list* p q more))))

(defn chainl1**
  "Modified version of blancas.kern.expr/chainl1* that returns a pipeit ast node"
  [node p op]
  (letfn [(rest [a] (<|> (bind [f op b p]
                           (rest (node f a b)))
                         (return a)))]
    (bind [a p] (rest a))))

(defn chainr1**
  "Modified version of blancas.kern.expr/chainr1* that returns a pipeit ast node"
  [node p op]
  (bind [a p]
        (<|> (bind [f op b (chainr1** node p op)]
                   (return (node f a b)))
             (return a))))

(defn prefix1**
  "Modified version of blancas.kern.expr/prefix1* that returns a pipeit ast node"
  [node p op]
  (<|> (bind [f op a (prefix1** node p op)]
             (return (node f a)))
       (bind [a p] (return a))))
