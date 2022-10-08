#lang racket
(require "parser.rkt")
(require data/either)

(define source-files '("input/Input01.txt"
                       "input/Input02.txt"
                       "input/Input03.txt"
                       "input/Input04.txt"
                       "input/Input05.txt"
                       "input/Input06.txt"
                       "input/Input07.txt"))

(for-each (lambda (file)
            (printf "~n======================== ~a ========================~n~n" file)
            (match (parse file)
              [(success result) (printf "Parsed successfully!~n~n") (pretty-print result)]
              [(failure context) (displayln context)]))
          source-files)

