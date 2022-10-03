#lang typed/racket/no-check
(provide tokenize)
(require data/either)

; Stole this from Bartosz Milewski
; https://www.schoolofhaskell.com/user/bartosz/basics-of-haskell/7-tokenizer-higher-order-functions
(define (span predicate lst)
  (define (span-acc accumulator elements)
    (if (empty? elements)
        (values accumulator '())
        (let ([head (first elements)]
              [tail (rest elements)])
          (if (predicate head)
              (let-values ([(acc1 tail1) (span-acc accumulator tail)])
                (values (cons head acc1) tail1))
              (values accumulator (cons head tail))))))
  (span-acc '() lst))


(define-type Token (U TokOp
                      TokIdent
                      TokKeyword
                      TokNum
                      TokAssign
                      TokLParen
                      TokRParen
                      'TokSpace
                      'TokNewline
                      'Unimplemented))

(struct TokAddOp ([op : Symbol] [offset : Number]) #:transparent)
(struct TokMultOp ([op : Symbol] [offset : Number]) #:transparent)
(struct TokIdent ([ident : Symbol] [offset : Number]) #:transparent)
(struct TokNum ([number : Number] [offset : Number]) #:transparent)
(struct TokKeyword ([kw : Keyword] [offset : Number]) #:transparent)
(struct TokAssign ([offset : Number]) #:transparent)
(struct TokLParen ([offset : Number]) #:transparent)
(struct TokRParen ([offset : Number]) #:transparent)
(struct TokEOF ([offset : Number]) #:transparent)


(define (char-alphanumeric? char)
  (or (char-alphabetic? char) (char-numeric? char)))

(define (tokenize-identifier head tail [offset 0])
  (let-values ([(str remaining) (span char-alphanumeric? tail)])
    (let ([result (cons head str)])
      (match (list->string (cons head str))
        ["read" (cons (TokKeyword '#:read offset) (tokenize remaining (+ offset 4)))]
        ["write" (cons (TokKeyword '#:write offset) (tokenize remaining (+ offset 5)))]
        [_ (cons (TokIdent (string->symbol (list->string result)) offset)
                 (tokenize remaining (+ offset (length result))))]))))

(define (tokenize-number head tail [offset 0])
  (let-values ([(str remaining) (span char-numeric? tail)])
    (let ([result (cons head str)])
      (cons (TokNum (string->number (list->string result)) offset)
            (tokenize remaining (+ offset (length result)))))))


(: tokenize (-> (Listof Char) (Listof Token)))
(define (tokenize chars [offset 0])
  (if (empty? chars)
      '()
      (let ([head (first chars)]
            [tail (rest chars)])

        (match head
          ;;; [#\space (cons 'TokSpace (tokenize tail (+ offset 1)))]
          ;;; [#\newline (cons 'TokNewline (tokenize tail (+ offset 1)))]
          ;;; [#\return (cons 'TokNewline (tokenize tail (+ offset 1)))]
          [(? char-whitespace?) (tokenize tail (+ offset 1))]
          [#\newline (tokenize tail (+ offset 1))]
          [#\return (tokenize tail (+ offset 1))]
          [#\: (if (eq? (first tail) #\=)
                   (cons (TokAssign offset) (tokenize (rest tail) (+ offset 2)))
                   (failure "Error: ':' must be followed by '='"))]
          [#\( (cons (TokLParen offset) (tokenize tail (+ offset 1)))]
          [#\) (cons (TokRParen offset) (tokenize tail (+ offset 1)))]
          [#\+ (cons (TokAddOp 'Plus offset) (tokenize tail (+ offset 1)))]
          [#\- (cons (TokAddOp 'Minus offset) (tokenize tail (+ offset 1)))]
          [#\* (cons (TokMultOp 'Mult offset) (tokenize tail (+ offset 1)))]
          [#\/ (cons (TokAddOp 'Div offset) (tokenize tail (+ offset 1)))]
          [#\$ (if (eq? (first tail) #\$)
                   (TokEOF offset)
                   (failure "Error: '$' must be followed by '$'"))]
          [(? char-alphabetic? c) (tokenize-identifier c tail offset)]
          [(? char-numeric? c) (tokenize-number c tail offset)]
          [_ (failure (format "Error: unrecognized character '~a' at position ~a" head offset))]))))
