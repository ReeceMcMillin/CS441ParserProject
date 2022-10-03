#lang plai
;;; (provide (all-defined-out))

(require "utility.rkt")
(require megaparsack megaparsack/text)
(require data/monad data/applicative)
(require data/either)

(define declared-variables (make-parser-parameter '()))

;;; === Utility ===

;;; buffer-spaces/p wraps atoms to consume surrounding spaces.
;;; This prevents more complex expressions from having to worry about accidental space sensitivity.

;;; The (many/p space/p) pattern includes newlines, which would allow expressions to split across lines.
;;; We don't want that, so we match against #\space explicitly at the cost of flexibility.
(define (buffer-spaces/p parser)
  (do (many/p (char/p #\space))
    [p <- parser]
    (many/p (char/p #\space))
    (pure p)))

;;; === Operators ===

;;; add_op -> + | -
(define add-op/p
  (or/p (try/p (do (buffer-spaces/p (char/p #\+)) (pure +)))
        (do (buffer-spaces/p (char/p #\-)) (pure -))))

;;; mult_op -> * | /
(define mult-op/p
  (or/p (try/p (do (buffer-spaces/p (char/p #\*)) (pure *)))
        (do (buffer-spaces/p (char/p #\/)) (pure /))))

;;; === Keywords/Identifiers ===

(define read/p
  (buffer-spaces/p (list/p (string/p "read") (char/p #\space))))

(define write/p
  (buffer-spaces/p (list/p (string/p "write") (char/p #\space))))

;;; Identifier rules:
;;; - Must lead with either:
;;;     - a letter
;;;     - any number of underscores terminated by a letter
;;; - Must be followed by any number of letters or digits.
;;; Good: abc, a12, __private, _____superprivate
;;; Bad:  1a2, a-12, a1?2, __________1nvalid
(define identifier/p
  (do [nested <- (list/p (or/p letter/p
                               (list/p (many+/p (char/p #\_))
                                       letter/p))
                         (many/p (or/p letter/p digit/p)))]
    (lookahead/p (or/p space/p eof/p (string/p ")")))
    (pure (string->symbol (list->string (flatten nested))))))

;;; === More Complex Expressions ===

(define factor/p
  (or/p (try/p (do (string/p "(")
                 [expr <- expr/p]
                 (string/p ")")
                 (pure (factor expr))))
        identifier/p
        integer/p ))

;;; factor_tail -> mult_op factor factor_tail | epsilon
(define factor-tail/p
  (or/p (try/p (do [op <- mult-op/p]
                 [factor <- factor/p]
                 [tail <- factor-tail/p]
                 (pure (if (void? tail) (list op factor) (list op factor tail)))))
        void/p))

(define term/p
  (do [factor <- factor/p]
    [tail <- factor-tail/p]
    ;;; (pure (if (void? tail) (list factor) (list factor tail)))))
    (pure (term factor tail))))

(define term-tail/p
  (or/p (try/p (do [op <- add-op/p]
                 [term <- term/p]
                 [tail <- term-tail/p]
                 (pure (if (void? tail) (list op term) (list op term tail)))))
        void/p))

(define expr/p
  (do [term <- term/p]
    [tail <- term-tail/p]
    ;;; (pure (if (void? tail) (expr term '()) (expr term tail)))))
    (pure (expr term tail))))

(define declaration/p
  (do
      [id <- identifier/p]
    (many+/p space/p)
    (string/p ":=")
    (many+/p space/p)
    [expr <- expr/p]
    (pure (decl id expr))))

(define read-id/p
  (do read/p
    [id <- identifier/p]
    (pure (read id))))

(define write-expr/p
  (do write/p
    [expr <- expr/p]
    (pure (write expr))))

(define statement/p
  (or/p read-id/p
        write-expr/p
        declaration/p))

(define statement-list/p
  (or/p (try/p (do [head <- statement/p]
                 (many/p space/p)
                 [tail <- statement-list/p]
                 (pure (if (void? tail) head (flatten (list head tail))))))
        void/p))

(define program/p
  (do [stmt-list <- statement-list/p]
    (string/p "$$")
    (pure stmt-list)))

(define (parse filename)
  ;;; (parse-string add-op/p "+"))
  (define file (open-input-file filename))
  (define input (port->string file))
  (define lines (port->lines (open-input-file filename)))

  (define parsed (parse-string program/p input))
  (match parsed
    [(success _) parsed]
    [(failure err) (build-error-context err lines)]))
