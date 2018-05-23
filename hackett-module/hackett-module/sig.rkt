#lang racket/base

(provide sig
         Π
         val
         type
         def-signature)

(require syntax/parse/define
         "rep/sig-literals.rkt"
         hackett/private/type-language
         (only-in hackett/base type)
         (for-syntax racket/base
                     syntax/parse
                     hackett/private/util/stx
                     "rep/sig.rkt"
                     ))

(define-syntax val #f)

(begin-for-syntax
  (define-syntax-class sig-entry
    #:attributes [id norm]
    #:literals [val type]
    #:datum-literals [: =]
    [pattern (val id:id : {~type val-type:expr})
      #:with norm #'(#%val-decl val-type)]
    [pattern (type {~type id:id})
      #:with norm #'(#%type-decl (#%opaque))]
    [pattern (type {~type id:id} = {~type alias-type:expr})
      #:with norm #'(#%type-decl (#%alias alias-type))]))

(define-syntax-parser sig
  [(_ ent ...)
   #:with [entry:sig-entry ...] ((make-syntax-introducer #t) #'[ent ...])
   #`(#%sig
      #,(for/hash ([id (in-list (attribute entry.id))])
          (values (syntax-e id) id))
      #,(for/hash ([id (in-list (attribute entry.id))]
                   [norm (in-list (attribute entry.norm))])
          (values (syntax-e id) norm)))])

(define-syntax-parser Π
  #:datum-literals [:]
  [(_ ([x : A:expr]) B:expr)
   #'(#%pi-sig ([x A]) B)])

(define-simple-macro
  (def-signature name sig-expr:sig)
  (begin
    (define-values [] sig-expr.residual)
    (define-syntax name
      (make-variable-like-transformer
       #'sig-expr.expansion))))
