#lang typed/racket

(: char-paren? (-> Char Boolean))
(define (char-paren? char)
  (if (member char (string->list "()")) #t #f))

(define (parse-paren char)
  (match char
    [#\( 'TokLParen]
    [#\) 'TokRParen]))

(define-type Token (U TokOp
                      TokIdent
                      TokNum
                      'TokSpace
                      'TokAssign
                      'TokLParen
                      'TokRParen
                      'TokEnd))

(struct TokOp ([op : Symbol]))
(struct TokIdent ([ident : Char]))
(struct TokNum ([number : Number]))


(: tokenize (-> (Listof Char) (Listof Token)))
(define (tokenize chars)
  (if (empty? chars)
      '()
      (let ([head (first chars)]
            [tail (rest chars)])
        (printf "chars: ~v~n" chars)
        (printf "head: ~v~n" head)
        (printf "tail: ~v~n~n" tail)

        (match head
          [#\space (append (list 'TokSpace) (tokenize tail))]
          [#\= (append (list 'TokAssign) (tokenize tail))]
          [(? char-paren? c) (append (list (parse-paren head)) (tokenize tail))]
          [(? char-alphabetic? c) (append (list (TokIdent head)) (tokenize tail))]
          [(? char-numeric? c) (append (list (TokNum (char->integer head))) (tokenize tail))]))))

(define input "(a) = 3")
(define chars (string->list input))
(printf "chars: ~v~n" chars)

(tokenize chars)

(char-paren? #\a)
(char-alphabetic? #\a)

;;; (tokenize '( #\( #\) ))