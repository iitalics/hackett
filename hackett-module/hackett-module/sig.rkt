#lang racket/base

(provide (signature-out sig
                        Π)
         val
         type
         data
         module
         def-signature)

(require syntax/parse/define
         "rep/sig-literals.rkt"
         "namespace/reqprov.rkt"
         (except-in hackett/private/type-language
                    ~type
                    type-namespace-introduce
                    value-namespace-introduce)
         hackett/private/type-reqprov
         (only-in hackett/private/adt data-constructor-spec)
         (only-in hackett/base type data)
         (for-syntax racket/base
                     syntax/parse
                     (only-in syntax/parse [attribute @])
                     syntax/parse/experimental/template
                     hackett/private/util/stx
                     "rep/sig.rkt"
                     "namespace/namespace.rkt"
                     "util/hash.rkt"))


(define-syntax val #f)
(define-syntax module #f)

(begin-for-syntax
  (define-literal-set sig-surface-literals
    #:datum-literals [: =]
    [val type data module])

  (define-syntax-class sig-entry
    #:attributes [[key 1] [id 1] [decl 1]]
    #:literal-sets [sig-surface-literals]
    [pattern (val x:id : {~type val-type:expr})
      #:with [key ...]  #`[#,(namespaced:value #'x)]
      #:with [id ...]   #'[x]
      #:with [decl ...] #'[(#%val-decl val-type)]]
    [pattern (data {~type X:id} c:data-constructor-spec ...)
      #:with X-key (namespaced:type (attribute X))
      #:with [c-key ...] (map namespaced:value (attribute c.tag))
      #:with [c-type ...] (type-namespace-introduce
                           (template [(?->* c.arg ... X) ...]))
      #:with [[key id decl] ...]
      #`[[X-key X (#%type-decl (#%data c.tag ...))]
         [c-key c.tag (#%constructor-decl c-type)]
         ...]]
    [pattern (type {~type X:id})
      #:with [key ...]  #`[#,(namespaced:type #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%opaque))]]
    [pattern (type {~type X:id} = {~type alias-type:expr})
      #:with [key ...]  #`[#,(namespaced:type #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%alias alias-type))]]
    [pattern (module {~module X:id} : {~signature signature:expr})
      #:with [key ...]  #`[#,(namespaced:module #'X)]
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%module-decl signature)]])

  (define-syntax-class sig-entries
    #:attributes [[key 1] [id 1] [decl 1]]
    [pattern [entry:sig-entry ...]
      #:with [key ...] #'[entry.key ... ...]
      #:with [id ...] #'[entry.id ... ...]
      #:with [decl ...] #'[entry.decl ... ...]]))



(define-syntax-parser sig
  [(_ {~value ent} ...)
   #:with entries:sig-entries ((make-syntax-introducer #t) #'[ent ...])
   (define keys (map syntax->datum (attribute entries.key)))
   #`(#%sig
      #,(hash-zip keys (attribute entries.id))
      #,(hash-zip keys (attribute entries.decl)))])

(define-syntax-parser Π
  #:datum-literals [:]
  [(_ ([{~module x} : A:expr]) B:expr)
   #'(#%pi-sig ([x A]) B)])

(define-simple-macro
  (def-signature {~signature name} {~signature sig-expr:sig})
  (begin
    (define-values [] sig-expr.residual)
    (define-syntax name
      (make-variable-like-transformer
       #'sig-expr.expansion))))
