#lang racket
(require "parser.rkt")
(require data/either)
(require megaparsack/text)
(require rackunit)


(check-match (parse "input/Input01.txt") (success _))
(check-match (parse "input/Input02.txt") (failure _))
(check-match (parse "input/Input03.txt") (failure _))
(check-match (parse "input/Input04.txt") (success _))
(check-match (parse "input/Input05.txt") (success _))
(check-match (parse "input/Input06.txt") (failure _))


(check-match (parse-string identifier/p "abc") (success 'abc))
(check-match (parse-string identifier/p "a12") (success 'a12))
(check-match (parse-string identifier/p "1a2") (failure _))
(check-match (parse-string identifier/p "a-12") (failure _))
(check-match (parse-string identifier/p "a1?2") (failure _))
(check-match (parse-string identifier/p "__private") (success '__private))
(check-match (parse-string identifier/p "_____superprivate") (success '_____superprivate))
(check-match (parse-string identifier/p "__________1nvalid") (failure _))


(check-match (parse-string declaration/p "abc := 123") (success _))
(check-match (parse-string term-tail/p "- (B * A)") (success _))
(check-match (parse-string expr/p "(B - 2) + A - (B * A)") (success _))
(check-match (parse-string factor/p "(A + 1)") (success _))
(check-match (parse-string factor/p "((((((((A + 1))))))))") (success _))
(check-match (parse-string write-expr/p "write (A + 1) * (B - 2) + A - (B * A)") (success _))
(check-match (parse-string write-expr/p "write A - (B * A)") (success _))
