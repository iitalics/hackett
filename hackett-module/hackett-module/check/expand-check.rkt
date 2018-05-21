#lang racket/base
(provide
 attach-sig
 sig⇒
 make-module-var-transformer)

(require
 hackett/private/util/stx)

(define (attach-sig stx s)
  (syntax-property stx 'sig: s))

(define (sig⇒ stx [ctx #f])
  (define m-
    (local-expand stx 'module-begin '() ctx))
  (define sig
    (syntax-local-introduce
     (syntax-property m- 'sig:)))
  (list m- sig))

;; Id Sig -> Transformer
(define (make-module-var-transformer x s)
  (make-variable-like-transformer
   ; Adjust source location information before calling attach-type so that tooltips end up in the
   ; right place.
   (λ (stx) (attach-sig (replace-stx-loc x stx) s))))
