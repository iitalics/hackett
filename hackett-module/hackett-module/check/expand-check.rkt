#lang racket/base
(provide
 sig⇒
 sig⇐
 (all-from-out "expand-check-prop.rkt"))

(require
 syntax/parse
 hackett/private/util/stx
 "expand-check-prop.rkt"
 "sig-matches.rkt"
 (for-template "../rep/sig-literals.rkt"))

(define (sig⇒ m [ctx #f])
  (define m-
    (local-expand m 'module-begin '() ctx))
  (define sig
    (syntax-local-introduce
     (detach-sig m-)))
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
