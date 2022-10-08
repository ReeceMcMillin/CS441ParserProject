#lang racket
(require "parser.rkt")
(require data/either)

(define fuzz-path (format "~ainput/tests" (current-directory)))
(define fuzz-files (map (compose (curry format "input/tests/~a")
                                 path->string)
                        (directory-list fuzz-path)))

(for-each (lambda (file)
            (match (parse file)
              [(success result) (printf "~a parsed successfully!~n~n" file)]
              [(failure context) (displayln context) (exit)]
              ))

          fuzz-files)
