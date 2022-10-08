#lang racket
(provide (all-defined-out))

(require racket/struct)
(require megaparsack)
(require data/either)

(struct read (id)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'read)
      (lambda (obj) (list (read-id obj)))))])

(struct write (expr)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'write)
      (lambda (obj) (list (write-expr obj)))))])

;;; expr -> term term_tail
(struct expr (term tail)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'expr)
      (lambda (obj) (list (expr-term obj) (expr-tail obj)))))])

;;; factor -> ( expr ) | id | number
(struct factor (expr)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'factor)
      (lambda (obj) (list (factor-expr obj)))))])

;;; term -> factor factor_tail
(struct term (factor tail)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'term)
      (lambda (obj) (list (term-factor obj) (term-tail obj)))))])

(struct decl (id expr)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'decl)
      (lambda (obj) (list (decl-id obj) (decl-expr obj)))))])

;;; A parser without friendly, human-readable error messages isn't worth using!
;;;
;;; This function builds an error context over a failed parse, reconstructing the
;;; surrounding context of the error location with arrows pointing to the unexpected token.
(define (build-error-context err lines)
  (define loc (message-srcloc err))
  (define line-no (srcloc-line loc))
  (define error-line (list-ref lines (- line-no 1)))
  (define overline (list->string (append (make-list (srcloc-column loc) #\space)
                                         (make-list (srcloc-span loc) #\↓))))
  (define underline (list->string (append (make-list (srcloc-column loc) #\space)
                                          (make-list (srcloc-span loc) #\↑))))
  ;;; (printf "error line = ~a~n" line-no)
  ;;; (printf "num lines = ~a~n" (length lines))

  (if (eq? line-no 1)
      (failure (string-append* (parse-error->string err)
                               (list "\n\n"
                                     "    " overline "\n"
                                     (number->string (- line-no 1)) " | " error-line "\n"
                                     "  ⋮ " underline "\n"
                                     (number->string (+ line-no 1)) " | " (list-ref lines line-no) "\n")))

      (failure (string-append* (parse-error->string err)
                               (list "\n\n"
                                     (number->string (- line-no 1)) " | " (list-ref lines (- line-no 2)) "\n"
                                     "  ⋮ " overline "\n"
                                     (number->string line-no) " | " error-line "\n"
                                     "  ⋮ " underline "\n"
                                     (number->string (+ line-no 1)) " | " (list-ref lines line-no) "\n")))))
