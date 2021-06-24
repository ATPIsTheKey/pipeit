(ns pipeit.parse
  (:use [blancas.kern core])
  (:use [pipeit.utils]))

; todo: documentation

;; +-------------------------------------------------------------+
;; |                      Lexer definitions.                     |
;; +-------------------------------------------------------------+

(def integer-literal
  (<$> (comp (partial hash-map :type :integer-literal, :value) #(Integer/parseInt ^String %))
       (<+> (many1 digit))))

(def real-literal
  (<$> (comp (partial hash-map :type :real-literal, :value) #(Float/parseFloat ^String %))
       (<+> (many0 digit) (sym* \.) (many1 digit))))

(def symbol-literal
  (<$> (comp (partial hash-map :type :symbol-literal, :value) keyword)
       (<+> (skip (sym* \#)) (many1 (none-of* "{}[]();#, ")))))

(def string-literal
  (<$> (partial hash-map :type :string-literal, :value)
       (<+> (skip (sym* \")) (many0 (none-of* "\"")) (skip (sym* \")))))

(def bool-literal
  (<$> (comp (partial hash-map :type :bool-literal, :value) {"true" true, "false" false})
       (token* "true" "false")))

(def nil-literal
  (<$> (constantly {:type :nil-literal, :value nil})
       (token* "nil")))

(def identifier
  (<$> (partial hash-map :type :identifier, :value)
       (<+> letter (many0 (<|> alpha-num (one-of* "_.#$?"))))))

(def single-char-operator
  (<$> (partial hash-map :type :special, :value)
       (token* "+" "-" "%" "*" "/" "<" ">")))

(def double-char-operator
  (<$> (partial hash-map :type :special, :value)
       (token* "<|" "|>" "<=" ">=" "==" "**" "//")))

(def triple-char-operator
  (<$> (partial hash-map :type :special, :value)
       (token* "<?|" "|?>" "<*|" "|*>" "<.." "..>")))

(def quadruple-char-operator
  (<$> (partial hash-map :type :special, :value)
       (token* "not=" "<*.." "..*>" "<*?|" "|?*>")))

(def single-char-delim
  (<$> (partial hash-map :type :delim, :value)
       (token* "|" "<-" "->" "{" "}" "(" ")" "[" "]" ";" "," "." "=" ":")))

(def double-char-delim
  (<$> (partial hash-map :type :delim, :value)
       (token* "<-" "->")))

(def reserved
  (<$> (partial hash-map :type :reserved, :value)
       (token* "def" "let" "in" "if" "else" "cond" "otherwise" "monad" "do" "return" "for" "where" "recur")))

(def ws
  (one-of* " \t\r\n"))

(defn <|:>
  ([p q] (<|> (<:> p) (<:> q)))
  ([p q & more] (reduce <|:> (list* p q more))))

(def lexeme
  (<|:> real-literal
        integer-literal
        string-literal
        symbol-literal
        bool-literal
        nil-literal
        reserved
        quadruple-char-operator
        triple-char-operator
        double-char-operator
        double-char-delim
        single-char-operator
        single-char-delim
        identifier))

(def many-lexemes
  (sep-end-by (many0 ws) lexeme))

(defn lexeme-type [type & more]
  (<$> :value
       (satisfy #(in? (list* type more) (:type %)))))

(defn lexeme-value [value & more]
  (<$> :value
       (satisfy #(in? (list* value more) (:value %)))))

;; +-------------------------------------------------------------+
;; |                     Parser definitions.                     |
;; +-------------------------------------------------------------+

(declare let-expr fun-expr if-expr when-expr tuple-expr list-expr set-expr map-expr op-expr application-expr atom-expr)

(def expr
  (<|:> (fwd let-expr)
        (fwd fun-expr)
        (fwd if-expr)
        (fwd when-expr)
        (fwd tuple-expr)
        (fwd list-expr)
        (fwd set-expr)
        (fwd map-expr)
        (fwd op-expr)
        (fwd application-expr)
        (fwd atom-expr)))

(defn parentheses [p]
  (between (lexeme-value "(") (lexeme-value ")") p))

(defn brackets [p]
  (between (lexeme-value "[") (lexeme-value "]") p))

(defn curly-brackets [p]
  (between (lexeme-value "{") (lexeme-value "}") p))

(def literal-expr
  (<$> (partial hash-map :node-type :literal-expr, :value)
       (<|> (lexeme-type :integer-literal)
            (lexeme-type :real-literal)
            (lexeme-type :symbol-literal)
            (lexeme-type :string-literal)
            (lexeme-type :bool-literal)
            (lexeme-type :nil-literal))))

(def identifier-expr
  (<$> (partial hash-map :node-type :identifier-expr, :value)
       (lexeme-type :identifier)))

(def atom-expr
  (<|> literal-expr
       identifier-expr
       (parentheses expr)))

(def comprehension
  (let [input-set (bind [var (lexeme-type :identifier)
                         _ (lexeme-value "in")
                         set expr]
                    (return {:variable var,
                             :set      set}))]
    (bind [_ (lexeme-value "for")
           input-sets (sep-end-by1 (lexeme-value ",") input-set)
           _ (optional (lexeme-value "where"))
           predicates (sep-end-by (lexeme-value ",") expr)]
      (return {:input-sets input-sets,
               :predicates predicates}))))

(def tuple-expr
  (parentheses (bind [e expr
                      _ (lexeme-value ",")
                      es (sep-end-by (lexeme-value ",") expr)
                      compr (optional comprehension)]
                 (return {:node-type     :tuple-expr,
                          :exprs         (apply conj [e] es),
                          :comprehension compr}))))

(def list-expr
  (brackets (bind [es (sep-end-by (lexeme-value ",") expr)
                   compr (optional comprehension)]
              (return {:node-type     :list-expr,
                       :exprs         es,
                       :comprehension compr}))))

(def set-expr
  (curly-brackets (bind [es (sep-end-by (lexeme-value ",") expr)
                         compr (optional comprehension)]
                    (return {:node-type     :set-expr,
                             :exprs         es,
                             :comprehension compr}))))

(def map-expr
  (let [key-value-pair (bind [key expr
                              _ (lexeme-value ":")
                              value expr]
                         (return {:key   key,
                                  :value value}))]
    (curly-brackets (bind [key-value-pairs (sep-end-by (lexeme-value ",") key-value-pair)
                           compr (optional comprehension)]
                      (return {:node-type       :map-expr,
                               :key-value-pairs key-value-pairs,
                               :comprehension   compr})))))

(def fun-expr
  (bind [_ (lexeme-value "|")
         params (many0 (lexeme-type :identifier))
         _ (lexeme-value "|")
         _ (lexeme-value "->")
         body-expr expr]
    (return {:node-type :fun-expr,
             :params    params,
             :body-expr body-expr})))

(def assignment
  (bind [init-name (lexeme-type :identifier)
         _ (lexeme-value "=")
         init-expr expr]
    (return {init-name init-expr})))

(def let-expr
  (bind [_ (lexeme-value "let")
         assignments (sep-end-by (lexeme-value ",") assignment)
         _ (lexeme-value "in")
         body-expr expr]
    (return {:node-type   :let-expr,
             :assignments (apply merge assignments),
             :body-expr   body-expr})))

(defn chainl1**
  "Modified version of blancas.kern.expr/chainl1* that returns a pipeit ast node"
  [node-type p op]
  (letfn [(rest [a] (<|> (bind [f op b p]
                           (rest {:node-type node-type,
                                  :op        f,
                                  :lhs-expr  a,
                                  :rhs-expr  b}))
                         (return a)))]
    (bind [a p] (rest a))))

(defn chainr1**
  "Modified version of blancas.kern.expr/chainr1* that returns a pipeit ast node"
  [node-type p op]
  (bind [a p]
    (<|> (bind [f op b (chainr1** node-type p op)]
           (return {:node-type node-type,
                    :op        f,
                    :lhs-expr  a,
                    :rhs-expr  b}))
         (return a))))

(defn prefix1**
  "Modified version of blancas.kern.expr/prefix1* that returns a pipeit ast node"
  [node-type p op]
  (<|> (bind [f op a (prefix1** node-type p op)]
         (return {:node-type node-type,
                  :op        f,
                  :expr      a}))
       (bind [a p] (return a))))

(def application-expr
  (bind [e atom-expr
         es (many1 atom-expr)]
    (return (reduce #(hash-map :node-type :application-expr,
                               :fun-expr %1,
                               :arg-expr %2)
                    (list* e es)))))

(def op-expr
  (let [op-order (list {:ops (lexeme-value "or") :assoc :left}
                       {:ops (lexeme-value "and") :assoc :left}
                       {:ops (lexeme-value "not") :assoc :unary}
                       {:ops (lexeme-value "==" "not=" "<" ">" "<=" ">=") :assoc :left}
                       {:ops (lexeme-value "|>" "<|" "|*>" "<*|" "|?>" "<?|" "|*?>" "<?*|") :assoc :left}
                       {:ops (lexeme-value "..>" "<.." "..*>" "<*.." "..?>" "<?.." "..*?>" "<?*..") :assoc :left}
                       {:ops (lexeme-value "+" "-") :assoc :right}
                       {:ops (lexeme-value "*" "/" "//" "%") :assoc :left}
                       {:ops (lexeme-value "-" "+") :assoc :unary}
                       {:ops (lexeme-value "**") :assoc :right})]
    (->> op-order
         (map (fn [{ops :ops, assoc :assoc}]
                (case assoc
                  :left #(chainl1** :binary-expr % ops)
                  :right #(chainr1** :binary-expr % ops)
                  :unary #(prefix1** :unary-expr % ops))))
         (reduce comp)
         (bind-to (<|:> application-expr atom-expr)))))

(def if-expr
  (bind [conseq-expr op-expr
         _ (lexeme-value "if")
         pred-expr op-expr
         _ (lexeme-value "else")
         alt-expr op-expr]
    (return {:node-type   :if-expr,
             :pred-expr   pred-expr,
             :conseq-expr conseq-expr,
             :alt-expr    alt-expr})))

(def when-expr
  (bind [conseq-expr op-expr
         _ (lexeme-value "when")
         pred-expr op-expr]
    (return {:node-type   :when-expr,
             :pred-expr   pred-expr,
             :conseq-expr conseq-expr})))

(def def-stmt
  (bind [_ (lexeme-value "def")
         assignments (sep-end-by1 (lexeme-value ",") assignment)
         _ (lexeme-value ";")]
    (return {:node-type   :def-stmt,
             :assignments (apply merge assignments)})))

(def stmt
  (<|:> def-stmt
        (<< expr (lexeme-value ";"))))

(def many-stmts
  (many0 stmt))

(defn run-parse-string [s]
  (->> s
       (value many-lexemes)
       (run many-stmts)))

(defn parse-string [s]
  (->> s
       (value many-lexemes)
       (value many-stmts)))

(def parse-source-file
  (comp parse-string slurp))
