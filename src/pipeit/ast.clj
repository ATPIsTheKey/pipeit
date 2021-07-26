(ns pipeit.ast)

(defrecord Program [stmts])

(defrecord DefStmt [declarations])

(defrecord LetExpr [declarations rec? body-expr])

(defrecord FunExpr [params body-expr])

(defrecord IfExpr [pred-expr conseq-expr alt-expr])

(defrecord BinaryExpr [op lhs-expr rhs-expr])

(defrecord UnaryExpr [op expr])

(defrecord ApplicationExpr [fun-expr arg-expr])

(defrecord SeqExpr [type exprs comprehension])

(defrecord MapExpr [key-val-pairs comprehension])

(defrecord AtomExpr [type val])

(defrecord ConstPattern [])

(defrecord VarPattern [expr])

(defrecord AsPattern [pattern id])

(defrecord OrPattern [patterns])

(defrecord AndPattern [patterns])

(defrecord ConsPattern [patterns])

(defrecord ListPattern [patterns])

(defrecord TuplePattern [patterns])

(defrecord WildcardPattern [])

(defrecord TypePattern [pattern type])
