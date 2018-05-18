#lang racket/base
(require
 "mod.rkt"
 "rep/sig.rkt"
 racket/pretty
 syntax/parse/define
 hackett/private/type-language
 (prefix-in hkt: hackett/base)
 (prefix-in sig: "sig.rkt")
 (for-syntax racket/base
             syntax/parse))

(provide
 def-module)

(begin-for-syntax

  (define (sig⇒ stx)
    (define m-
      (local-expand stx 'module-begin '()))
    (define sig
      (syntax-local-introduce
       (syntax-property m- 'sig:)))
    (list m- sig))

  )

(define-syntax-parser def-module
  [(_ name:id m:expr)
   #:with [m- sig] (sig⇒ #'m)
   #'(begin
       (printf "inferred: ")
       (pretty-write 'sig)
       (printf "\nexpanded module body: ")
       (pretty-write 'm-))])
