#lang racket/base

(provide
 prop:dot-origin             dot-origin             dot-origin?
 dot-origin-internal-id
 prop:dot-accessible/value   dot-accessible/value   dot-accessible/value?
 prop:dot-accessible/pattern dot-accessible/pattern dot-accessible/pattern?
 prop:dot-accessible/type    dot-accessible/type    dot-accessible/type?
 prop:dot-accessible         dot-accessible         #| no predicate |#
 )

(require struct-like-struct-type-property)

;; internal-id : Id
;; this is the id that `reintroduce-#%dot` uses to compare

(define-struct-like-struct-type-property dot-origin
  [internal-id])

;; key->id : Key -> [Maybe Id]
;; such that the id will expand to someting that has/is the
;; correct type already

(define-struct-like-struct-type-property dot-accessible/value
  [key->id])

(define-struct-like-struct-type-property dot-accessible/pattern
  [key->id])

(define-struct-like-struct-type-property dot-accessible/type
  [key->id])

(define-struct-like-struct-type-property dot-accessible
  [internal-id
   value-key->id
   pattern-key->id
   type-key->id]
  #:property prop:dot-origin
  (λ (self) (dot-origin (dot-accessible-internal-id self)))
  #:property prop:dot-accessible/value
  (λ (self) (dot-accessible/value (dot-accessible-value-key->id self)))
  #:property prop:dot-accessible/pattern
  (λ (self) (dot-accessible/pattern (dot-accessible-pattern-key->id self)))
  #:property prop:dot-accessible/type
  (λ (self) (dot-accessible/type (dot-accessible-type-key->id self))))

