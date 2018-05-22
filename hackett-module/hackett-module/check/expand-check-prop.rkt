#lang racket/base
(provide
 attach-sig
 detach-sig)

(define (attach-sig stx s)
  (syntax-property stx 'sig: s))

(define (detach-sig stx-)
  (syntax-property stx- 'sig:))
