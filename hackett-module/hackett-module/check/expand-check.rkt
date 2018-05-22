#lang racket/base
(provide
 attach-sig
 sig⇒
 sig⇐)

(require
 syntax/parse
 hackett/private/util/stx
 "sig-matches.rkt"
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

;; assume expected is already expanded
(define (sig⇐ stx expected [ctx #f])
  (define/syntax-parse [m- actual] (sig⇒ stx ctx))

  (unless (signature-matches? #'actual expected)
    (raise-syntax-error #f
      (format "signature mismatch\n  expected: ~a\n  given:    ~a"
              (sig->string expected) (sig->string #'actual))
      stx))
  #'m-)

