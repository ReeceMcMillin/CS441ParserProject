#lang plait

;;; (define-type Token
;;;   (TokOp    [op : Symbol])
;;;   (TokAssign)
;;;   (TokLParen)
;;;   (TokRParen)
;;;   (TokIdent [ident : Symbol])
;;;   (TokNum [number : Number])
;;;   (TokEnd))

;;; (define (tokenize str)
;;;   (match str
;;;     [(list head rest ...)] head))

;;; (tokenize "abcd")