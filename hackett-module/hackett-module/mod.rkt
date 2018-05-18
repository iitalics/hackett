#lang racket/base
(require
 "rep/sig.rkt"
 racket/pretty
 syntax/parse/define
 hackett/private/type-language
 (prefix-in hkt: hackett/base)
 (prefix-in sig: "sig.rkt")
 (for-syntax racket/base
             syntax/parse))

(provide
 mod)

(begin-for-syntax
  (define-literal-set mod-stop-literals
    #:literal-sets [kernel-literals]
    [hkt:: hkt:type])

  (define mod-stop-ids
    (list #'hkt::
          #'hkt:type
          #'define-values
          #'define-syntaxes
          #'begin
          #'#%require
          #'#%provide
          ))
  )

(define-syntax-parser mod/acc
  [(_ [sig-entry/rev ...])
   #:with [sig-entry ...] (reverse (attribute sig-entry/rev))
   #:with s:sig #'(sig:sig sig-entry ...)
   #'(pretty-write 's.expansion)]

  [(_ [ent/rev ...] defn rest-defn ...)
   #:with defn- (local-expand #'defn 'module mod-stop-ids)
   (syntax-parse #'defn-
     #:literal-sets [mod-stop-literals]

     [(begin form ...)
      #'(mod/acc [ent/rev ...] form ... rest-defn ...)]

     [({~or define-values
            define-syntaxes
            #%require
            #%provide
            } . _)
      #'(begin defn- (mod/acc [ent/rev ...] rest-defn ...))]

     [(hkt:: id:id {~type type:expr} {~optional #:exact})
      #'(begin defn- (mod/acc [(sig:val id : type) ent/rev ...] rest-defn ...))]

     [(hkt:type {~type id:id} {~type rhs:expr})
      #'(begin defn- (mod/acc [(sig:type id = rhs) ent/rev ...] rest-defn ...))])])

(define-syntax-parser mod
  [(_ defn ...)
   #'(#%module-begin
      (mod/acc
       []
       defn ...))])
