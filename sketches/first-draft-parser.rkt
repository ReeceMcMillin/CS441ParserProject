#lang racket
(provide (all-defined-out))

(require megaparsack megaparsack/text)
(require data/monad data/applicative)

;;; === Operators ===

;;; add_op -> + | -
(define add-op/p
  (one-of/p '(#\+ #\-)))

;;; mult_op -> * | /
(define mult-op/p
  (one-of/p '(#\* #\/)))

;;; === Keywords/Identifiers ===

(define read/p
  (string/p "read "))

(define write/p
  (string/p "write "))

(define identifier/p
  (do [nested <- (list/p (or/p letter/p
                               (many+/p (char/p #\_)))
                         (many/p (or/p letter/p digit/p)))]
    (lookahead/p (or/p space/p eof/p))
    (pure (string->symbol (list->string (flatten nested))))))

;;; === More Complex Expressions ===

;;; factor -> ( expr ) | id | number
(define factor/p
  (or/p (list/p (string/p "(") expr/p (string/p ")") #:sep (many/p space/p))
        identifier/p
        integer/p))
;;; (or/p (try/p (do (string/p "(")
;;;                (many/p space/p)
;;;                [expr <- expr/p]
;;;                (many/p space/p)
;;;                (string/p ")")
;;;                (pure expr)))
;;;       identifier/p
;;;       integer/p ))

;;; factor_tail -> mult_op factor factor_tail | epsilon
(define factor-tail/p
  (or/p (try/p (do [op <- mult-op/p]
                 (many/p space/p)
                 [factor <- factor/p]
                 (many/p space/p)
                 [tail <- factor-tail/p]
                 (pure (if (void? tail) (list op factor) (list op factor tail)))))
        void/p))

(define term/p
  (list/p factor/p factor-tail/p #:sep (many/p space/p)))
;;; (do [factor <- factor/p]
;;;   (many/p space/p)
;;;   [tail <- factor-tail/p]
;;;   (pure (if (void? tail) factor (list factor tail)))))


(define term-tail/p
  (or/p (try/p (do (many/p space/p)
                 [op <- add-op/p]
                 (many/p space/p)
                 [term <- term/p]
                 (many/p space/p)
                 [tail <- term-tail/p]
                 (pure (if (void? tail) (list op term) (list op term tail)))))
        void/p))

(define expr/p
  (do (many/p space/p)
    [term <- term/p]
    (many/p space/p)
    [tail <- term-tail/p]
    (pure (if (void? tail) term (cons term tail)))))

(define declaration/p
  (do
      [id <- identifier/p]
    (many+/p space/p)
    (string/p ":=")
    (many+/p space/p)
    [expr <- expr/p]
    (pure (cons id expr))))

(define read-id/p
  (do read/p
    (many/p space/p)
    [id <- identifier/p]
    (pure (cons 'read id))))

(define write-expr/p
  (do write/p
    [expr <- expr/p]
    (pure (cons 'write expr))))

(define statement/p
  (or/p read-id/p
        write-expr/p
        declaration/p))

(define statement-list/p
  (or/p (try/p (do [head <- statement/p]
                 (many/p space/p)
                 [tail <- statement-list/p]
                 (pure (if (void? tail) head (cons head tail)))))
        void/p))

(define program/p
  (do (many/p space/p)
    [stmt-list <- statement-list/p]
    (many/p space/p)
    (string/p "$$")
    (pure stmt-list)))

(define (parse filename)
  (define file (open-input-file filename))
  (define input (port->string file))

  (parse-string program/p input))

