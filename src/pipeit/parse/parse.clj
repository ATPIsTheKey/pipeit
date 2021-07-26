(ns pipeit.parse.parse
  (:require [pipeit.ast :as ast])
  (:use [pipeit.utils])
  (:use [blancas.kern core expr])
  (:use [pipeit.parse.combinators]))

; todo: documentation

;; +-------------------------------------------------------------+
;; |                      Lexer definitions.                     |
;; +-------------------------------------------------------------+

(defrecord Lexeme [type val])

(def integer-literal
  (<$> (comp (partial ->Lexeme :integer-literal) #(Integer/parseInt ^String %))
       (<+> (many1 digit))))

(def real-literal
  (<$> (comp (partial ->Lexeme :real-literal) #(Float/parseFloat ^String %))
       (<+> (many0 digit) (sym* \.) (many1 digit))))

(def symbol-literal
  (<$> (comp (partial ->Lexeme :symbol-literal) keyword)
       (<+> (skip (sym* \#)) (many1 (none-of* "{}[]();#, ")))))

(def string-literal
  (<$> (partial ->Lexeme :string-literal)
       (<+> (skip (sym* \")) (many0 (none-of* "\"")) (skip (sym* \")))))

(def bool-literal
  (<$> (comp (partial ->Lexeme :bool-literal) {"true" true, "false" false})
       (token* "true" "false")))

(def nil-literal
  (<$> (constantly (->Lexeme :nil-literal nil))
       (token* "nil")))

(def identifier
  (<$> (partial ->Lexeme :ident)
       (<+> letter (many0 (<|> alpha-num (one-of* "_.#$?"))))))

(def single-char-operator
  (<$> (partial ->Lexeme :operator)
       (token* "+" "-" "%" "*" "/" "<" ">")))

(def double-char-operator
  (<$> (partial ->Lexeme :operator)
       (token* "<|" "|>" "<=" ">=" "==" "**" "//" "::")))

(def triple-char-operator
  (<$> (partial ->Lexeme :operator)
       (token* "<?|" "|?>" "<*|" "|*>" "<.." "..>")))

(def quadruple-char-operator
  (<$> (partial ->Lexeme :operator)
       (token* "not=" "<*.." "..*>" "<*?|" "|?*>")))

(def single-char-delim
  (<$> (partial ->Lexeme :delim)
       (token* "|" "&" "?" "{" "}" "(" ")" "[" "]" ";" "," "." "=" ":")))

(def double-char-delim
  (<$> (partial ->Lexeme :delim)
       (token* "<-" "->")))

(def reserved
  (<$> (partial ->Lexeme :reserved)
       (token* "def" "let" "in" "if" "match" "as" "then" "else" "for" "where" "recur")))

(def ws
  (one-of* " \t\r\n"))

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
  (<$> :val
       (satisfy #(in? (list* type more) (:type %)))))

(defn lexeme-val [val & more]
  (<$> :val
       (satisfy #(in? (list* val more) (:val %)))))

;; +-------------------------------------------------------------+
;; |                     Expression parser definitions.          |
;; +-------------------------------------------------------------+

(declare let-expr fun-expr match-expr if-expr tuple-expr list-expr set-expr map-expr op-expr application-expr atom-expr)

(def expr
  (<|:> (fwd let-expr)
        (fwd fun-expr)
        #_(fwd match-expr)
        (fwd if-expr)
        (fwd tuple-expr)
        (fwd list-expr)
        #_(fwd set-expr)
        #_(fwd map-expr)
        (fwd op-expr)
        (fwd application-expr)
        (fwd atom-expr)))

(defn parentheses [p]
  (between (lexeme-val "(") (lexeme-val ")") p))

(defn brackets [p]
  (between (lexeme-val "[") (lexeme-val "]") p))

(defn curly-brackets [p]
  (between (lexeme-val "{") (lexeme-val "}") p))

(def literal-expr
  (<$> (partial ast/->AtomExpr :literal)
       (<|> (lexeme-type :integer-literal)
            (lexeme-type :real-literal)
            (lexeme-type :symbol-literal)
            (lexeme-type :string-literal)
            (lexeme-type :bool-literal)
            (lexeme-type :nil-literal))))

(def ident-expr
  (<$> (partial ast/->AtomExpr :ident)
       (lexeme-type :ident)))

(def atom-expr
  (<|> literal-expr
       ident-expr
       (parentheses expr)))

(def comprehension
  (let [input-set (bind [var (lexeme-type :ident)
                         _ (lexeme-val "in")
                         set expr]
                    (return {:var var,
                             :set set}))]
    (bind [_ (lexeme-val "for")
           input-sets (sep-end-by1 (lexeme-val ",") input-set)
           _ (optional (lexeme-val "where"))
           preds (sep-end-by (lexeme-val ",") expr)]
      (return {:input-sets input-sets,
               :preds      preds}))))

(def tuple-expr
  (parentheses (bind [expr* expr
                      _ (lexeme-val ",")
                      exprs (sep-end-by (lexeme-val ",") expr)
                      compr (optional comprehension)]
                 (return (ast/->SeqExpr :tuple (apply conj [expr*] exprs) compr)))))

(def list-expr
  (brackets (bind [exprs (sep-end-by (lexeme-val ",") expr)
                   compr (optional comprehension)]
              (return (ast/->SeqExpr :list exprs compr)))))

(def set-expr
  (curly-brackets (bind [exprs (sep-end-by (lexeme-val ",") expr)
                         compr (optional comprehension)]
                    (return (ast/->SeqExpr :set exprs compr)))))

(def map-expr
  (let [key-val-pair (bind [key expr
                            _ (lexeme-val ":")
                            val expr]
                       (return {:key key,
                                :val val}))]
    (curly-brackets (bind [key-val-pair (sep-end-by (lexeme-val ",") key-val-pair)
                           compr (optional comprehension)]
                      (return (ast/->MapExpr key-val-pair compr))))))

(def fun-expr
  (bind [_ (lexeme-val "|")
         params (many0 (lexeme-type :ident))
         _ (lexeme-val "|")
         _ (lexeme-val "->")
         body-expr expr]
    (return (ast/->FunExpr params body-expr))))

(def declaration
  (bind [init-name (lexeme-type :ident)
         _ (lexeme-val "=")
         init-expr expr]
    (return {init-name init-expr})))

(def let-expr
  (bind [_ (lexeme-val "let")
         rec (optional (lexeme-val "rec"))
         declarations (sep-end-by (lexeme-val ",") declaration)
         _ (lexeme-val "in")
         body-expr expr]
    (return (ast/->LetExpr (apply merge declarations) ((complement nil?) rec) body-expr))))

(def application-expr
  (bind [fun atom-expr
         args (many1 atom-expr)]
    (return (reduce ast/->ApplicationExpr (list* fun args)))))

(def op-expr
  (let [op-order (list {:ops (lexeme-val "|>" "<|" "|*>" "<*|" "|?>" "<?|" "|*?>" "<?*|") :assoc :left}
                       {:ops (lexeme-val "or") :assoc :left}
                       {:ops (lexeme-val "and") :assoc :left}
                       {:ops (lexeme-val "not") :assoc :unary}
                       {:ops (lexeme-val "==" "not=" "<" ">" "<=" ">=") :assoc :left}
                       {:ops (lexeme-val "::") :assoc :right}
                       {:ops (lexeme-val "+" "-") :assoc :right}
                       {:ops (lexeme-val "*" "/" "//" "%") :assoc :left}
                       {:ops (lexeme-val "-" "+") :assoc :unary}
                       {:ops (lexeme-val "**") :assoc :right}
                       ; todo: make function composition highest precedence
                       {:ops (lexeme-val "..>" "<.." "..*>" "<*.." "..?>" "<?.." "..*?>" "<?*..") :assoc :left})]
    (->> op-order
         (map (fn [{ops :ops, assoc :assoc}]
                (case assoc
                  :left #(chainl1** ast/->BinaryExpr % ops)
                  :right #(chainr1** ast/->BinaryExpr % ops)
                  :unary #(prefix1** ast/->UnaryExpr % ops))))
         (reduce comp)
         (on (<|:> application-expr atom-expr)))))

(def if-expr
  (bind [_ (lexeme-val "if")
         pred-expr expr
         _ (lexeme-val "then")
         conseq-expr expr
         _ (optional (lexeme-val "else"))
         alt-expr (optional expr)]
    (return (ast/->IfExpr pred-expr conseq-expr alt-expr))))


;; +-------------------------------------------------------------+
;; |                      Pattern parser definitions.            |
;; +-------------------------------------------------------------+

(declare constant-pattern var-pattern as-pattern)

(def pattern
  (<|:> (fwd constant-pattern)
        (fwd var-pattern)
        (fwd as-pattern)
        (fwd constant-pattern)
        (fwd constant-pattern)))

(def constant-pattern
  (<$> ast/->ConstPattern
       literal-expr))

(def var-pattern
  (<$> ast/->VarPattern
       ident-expr))

(def as-pattern
  (bind [patt pattern
         _ (lexeme-val "as")
         ident identifier]
    (return (ast/->AsPattern patt ident))))

(def and-pattern
  (<$> ast/->AndPattern
       (sep-by1 (lexeme-val "&") pattern)))

(def or-pattern
  (<$> ast/->OrPattern
       (sep-by1 (lexeme-val "|") pattern)))

(def cons-pattern
  (<$> ast/->ConsPattern
       (chainr1 pattern (lexeme-val "::"))))

(def paren-pattern
  (parentheses pattern))

(def tuple-pattern
  (parentheses
    (bind [patt pattern
           _ (lexeme-val ",")
           patts (sep-end-by (lexeme-val ",") expr)]
      (return (ast/->TuplePattern (vec (cons patt patts)))))))

(def list-pattern
  (<$> ast/->ListPattern
    (brackets (sep-end-by (lexeme-val ",") pattern))))

(def type-pattern
  ())

(def wildcard-pattern
  (<$> (constantly (ast/->WildcardPattern))
       (lexeme-val "_")))

;; +-------------------------------------------------------------+
;; |                      Statement parser definitions.          |
;; +-------------------------------------------------------------+

(def def-stmt
  (bind [_ (lexeme-val "def")
         declarations (sep-end-by1 (lexeme-val ",") declaration)
         _ (lexeme-val ";")]
    (return (ast/->DefStmt declarations))))

(def stmt
  (<|:> def-stmt
        (<< expr (lexeme-val ";"))))

(def program
  (<$> ast/->Program
       (many0 stmt)))

(defn parse-string [s]
  (->> s
       (value many-lexemes)
       (value program)))

(def parse-source-file
  (comp parse-string slurp))
