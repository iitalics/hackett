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

  (define-syntax-class hackett-module-component
    #:literal-sets [mod-stop-literals]
    #:attributes [sig-entry]
    [pattern (hkt:: id:id {~type type:expr} {~optional #:exact})
             #:with sig-entry #'(sig:val id : type)]
    [pattern (hkt:type {~type spec} {~type rhs:expr})
             #:fail-unless (identifier? #'spec)
             "type aliases with arguments not allowed in modules"
             #:with sig-entry #'(sig:type id = rhs)])

  (define-syntax-class pass-through
    #:literal-sets [mod-stop-literals]
    [pattern ({~or define-values
                   define-syntaxes
                   #%require
                   #%provide
                   #%expression
                   }
              . _)])

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

     [d:hackett-module-component
      #'(begin defn- (mod/acc [d.sig-entry ent/rev ...] rest-defn ...))]

     [:pass-through
      #'(begin defn- (mod/acc [ent/rev ...] rest-defn ...))])])

(define-syntax-parser mod
  [(_ defn ...)
   #'(hkt:#%module-begin
      (mod/acc
       []
       defn ...))])
