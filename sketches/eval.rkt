;;; This isn't part of it!

#lang racket
(require "../parser.rkt" "../utility.rkt")
(require data/either)
(require megaparsack/text)

(define (eval-factor factor-token)
  (printf "matching factor-token ~v~n" factor-token)

  (match factor-token
    [(? number? tok) tok]
    [(? symbol? tok) tok]
    [_ (eval-expr factor-token)]))

(define (eval-term term-token)
  (printf "matching term-token ~v~n" term-token)
  (match term-token
    [(success (term factor (? void?))) (eval-factor factor)]
    [(success (term factor (list op tail)))
     (printf "saw (factor ~v) (op ~v) (tail ~v)~n" factor op tail)
     (op (eval-factor factor) tail)]))
;;; [(success (? number? num)) (list 'numforsomereason num)]))

(define (eval-expr expr-token)
  (printf "matching expr-token ~v~n" expr-token)
  (match expr-token
    [(success (expr (term factor (list op term)) tail))
     (op (eval-factor factor) term)]
    [(success tok) tok]))

(define test-term (parse-string term/p "2 + 3"))
(define test-expr (parse-string expr/p "2 * A"))

;;; (eval-term test-term)
;;; (void? (eval-expr test-expr))
(eval-expr test-expr)
