#lang racket/base
(require
 "rep/sig.rkt"
 racket/pretty
 syntax/parse/define
 hackett/private/type-language
 (prefix-in hkt: hackett/base)
 (prefix-in sig: "sig.rkt")
 (for-syntax racket/base
             syntax/parse
             syntax/kerncase
             syntax/id-set))

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

  (define kernel-id-set
    (immutable-free-id-set (kernel-form-identifier-list)))
  (define kernel-known-expr-id-set
    (immutable-free-id-set (list #'#%expression
                                 #'#%plain-lambda
                                 #'case-lambda
                                 #'if
                                 #'begin0 ; not begin!
                                 #'let-values
                                 #'letrec-values
                                 #'set!
                                 #'quote
                                 #'quote-syntax
                                 #'with-continuation-mark
                                 #'#%plain-app
                                 #'#%top
                                 #'#%variable-reference)))

  (define-syntax-class hackett-module-component
    #:literal-sets [mod-stop-literals]
    #:attributes [sig-entry]
    [pattern (hkt:: id:id {~type type:expr} {~optional #:exact})
             #:with sig-entry #'(sig:val id : type)]
    [pattern (hkt:type {~type spec} {~type rhs:expr})
             #:fail-unless (identifier? #'spec)
             "type aliases with arguments not allowed in modules"
             #:with sig-entry #'(sig:type id = rhs)])

  ;; Id -> Bool
  (define (variable-id? x)
    (define sym (gensym 'fail))
    ;; bound
    (and (identifier-binding x)
         ;; not a macro
         (eq? sym (syntax-local-value x (λ () sym)))
         ;; not a core form
         (not (free-id-set-member? kernel-id-set x))))

  (define-syntax-class pass-through
    #:literal-sets [mod-stop-literals]
    ;; pass-through these types of definitions
    [pattern ({~or define-values
                   define-syntaxes
                   }
              . _)]
    ;; pass-through known expressions
    [pattern x:id  #:when (variable-id? #'x)]
    [pattern (head:id . _)
             #:when
             (or (free-id-set-member? kernel-known-expr-id-set #'head)
                 (variable-id? #'head))])

  (define-syntax-class disallowed
    #:literal-sets [mod-stop-literals]
    [pattern ({~or #%provide
                   #%require
                   begin-for-syntax
                   module
                   module*
                   }
              . _)])

  )

; The mod/acc form parses and expands definitions using the "trampolining #%module-begin" technique;
; we insert `begin` expressions so that an outer `#%module-begin` will handle the introduction of
; bindings.
;
; When the outer module is fully expanded, `mod/acc` will leave behind ???

(begin-for-syntax
  (define mod/acc-sig-prop
    (gensym 'sig⇒))

  (define-syntax-class mod/acc-sig
    #:attributes [sig]
    [pattern stx
             #:attr sig (syntax-local-introduce
                         (syntax-property #'stx mod/acc-sig-prop))
             #:when (attribute sig)]))

; a module signature
(define-syntax-parser mod/acc
  [(_ [sig-entry/rev ...])
   #:with [sig-entry ...] (reverse (attribute sig-entry/rev))
   #:with s:sig #'(sig:sig sig-entry ...)
   (syntax-property #'(define-values [] (values))
     mod/acc-sig-prop
     (syntax-local-introduce
      (attribute s.expansion)))]

  [(_ [ent/rev ...] defn rest-defn ...)
   #:with defn- (local-expand #'defn 'module mod-stop-ids)
   (syntax-parse #'defn-
     #:literal-sets [mod-stop-literals]

     [(begin form ...)
      #'(mod/acc [ent/rev ...] form ... rest-defn ...)]

     [d:hackett-module-component
      #'(begin defn- (mod/acc [d.sig-entry ent/rev ...] rest-defn ...))]

     [:pass-through
      #'(begin defn- (mod/acc [ent/rev ...] rest-defn ...))]

     [{~and (head . _) :disallowed}
      (raise-syntax-error #f
        (format "~a form not allowed inside of a module"
                #'head)
        #'defn)])])

(define-syntax-parser mod
  [(_ defn ...)
   #:with modbeg- (local-expand #'(hkt:#%module-begin
                                   (mod/acc
                                    []
                                    defn ...))
                                'module-begin
                                '())
   #:with (mb-head:id
           pre-defn- ...
           m:mod/acc-sig
           post-defn- ...) #'modbeg-

   (syntax-property #'(let ()
                        pre-defn- ...
                        post-defn- ...
                        ;; TODO: fill in references to the definitions into the hash
                        (hash))
     'sig:
     (syntax-local-introduce
      (attribute m.sig)))])
