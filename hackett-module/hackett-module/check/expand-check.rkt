#lang racket/base
(provide
 attach-sig
 sig⇒)

(require
 syntax/parse
 hackett/private/util/stx
 (for-template "../rep/sig.rkt"))

(define (attach-sig stx s)
  (syntax-property stx 'sig: s))

(define (sig⇒ stx [ctx #f])
  (define m-
    (local-expand stx 'module-begin '() ctx))
  (define sig
    (syntax-local-introduce
     (syntax-property m- 'sig:)))
  (list m- sig))
