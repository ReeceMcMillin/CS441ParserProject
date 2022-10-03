#lang typed/racket/no-check
(provide (all-defined-out))
(require data/either)

(define (filter-tokens tokens)
  (filter (lambda (x)
            (not (or (eq? 'TokSpace x)
                     (eq? 'TokNewline x))))
          tokens))

(define (display-tokens tokens)
  (for-each (lambda (x) (println x))
            tokens))

(define file->chars (compose string->list file->string))

(define (tokenizer-error? tokens)
  (ormap (lambda (x) (failure? x)) tokens))

