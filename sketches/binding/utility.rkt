#lang racket
(provide (all-defined-out))

(require racket/struct)
(require megaparsack)

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


(define (build-error-context err lines)
  (define loc (message-srcloc err))
  (define line-no (srcloc-line loc))
  (define error-line (list-ref lines (- line-no 1)))
  (define overline (list->string (append (make-list (srcloc-column loc) #\space)
                                         (make-list (srcloc-span loc) #\↓))))
  (define underline (list->string (append (make-list (srcloc-column loc) #\space)
                                          (make-list (srcloc-span loc) #\↑))))
  (string-append* (parse-error->string err)
                  (list "\n\n"
                        (number->string (- line-no 1)) " | " (list-ref lines (- line-no 2)) "\n"
                        "  ⋮ " overline "\n"
                        (number->string line-no) " | " error-line "\n"
                        "  ⋮ " underline "\n"
                        (number->string (+ line-no 1)) " | " (list-ref lines line-no) "\n")))




