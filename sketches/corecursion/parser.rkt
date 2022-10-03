#lang racket
(require "tokenizer.rkt" "util.rkt" data/either)

(define (parse filename)
  (let* ([input (file->chars filename)]
         [tokens (tokenize input)])
    tokens))

(define toks (flatten (parse "input/Input05.txt")))

(if (tokenizer-error? toks)
    (display (format "~a~n" (from-failure #f (first (filter failure? toks)))))
    toks)

;;; (for-each (lambda (x) (println x)) toks)
;;; (ormap (lambda (x) (failure? x)) toks)

;;; (for-each (lambda (n)
;;;             (let* ([filename (format "input/Input0~a.txt" n)]
;;;                    [input (port->string (open-input-file filename))]
;;;                    [chars (string->list input)]
;;;                    [tokens (flatten (tokenize chars))]
;;;                    [filtered (filter-tokens tokens)])
;;;               (printf "============== ~a ==============~n" filename)
;;;               (display-tokens filtered)))
;;;           ;;; '(1))
;;;           '(1 2 3 4 5 6 7))