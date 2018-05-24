#lang racket/base
(require
 "rep/sig-literals.rkt"
 racket/pretty
 syntax/parse/define
 hackett/private/type-language
 (prefix-in hkt: hackett/base)
 (prefix-in sig: "sig.rkt")
 (for-syntax racket/base
             syntax/parse
             syntax/kerncase
             syntax/id-set
             "rep/sig.rkt"
             "check/expand-check.rkt"))

(provide
 mod)

(begin-for-syntax
  (define disappeared-binding 'disappeared-binding)

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
    #:attributes [sig-entry [val-id 1] residual]
    #:literal-sets [mod-stop-literals]
    ;; NOTE: we are not introducing the type namespace
    ;;   here, because they will be introduced by `sig:val`
    ;;   and `sig:type` (the surface syntax).
    [pattern (hkt:: id:id type:expr {~optional #:exact})
             #:with sig-entry #'(sig:val id : type)
             #:with [val-id ...] #'[id]
             #:with residual (syntax-property #'(values)
                                              disappeared-binding
                                              (syntax-local-introduce #'id))]
    [pattern (hkt:type spec rhs:expr)
             #:fail-unless (identifier? #'spec)
             "type aliases with arguments not allowed in modules"
             #:with sig-entry #'(sig:type spec = rhs)
             #:with [val-id ...] #'[]
             #:with residual (syntax-property #'(values)
                                              disappeared-binding
                                              (syntax-local-introduce #'spec))])

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
; we insert `begin` expressions so that the outer Racket expander will handle the introduction of
; bindings.
;
; When mod/acc is done parsing the definitions, it will leave behind an expression that generates
; a hash table for the module. This expression will have a `sig⇒` syntax property on it containing
; the inferred signature.

(begin-for-syntax
  (define mod/acc-sig-prop (gensym 'sig⇒))

  (define-syntax-class mod/acc-sig
    #:attributes [sig]
    ;; base case, when the expression with sig⇒ is found
    [pattern stx
             #:when (syntax-property #'stx mod/acc-sig-prop)
             #:attr sig (syntax-local-introduce
                         (syntax-property #'stx mod/acc-sig-prop))]

    ;; recursive case for an intermediate binding expression such as
    ;; (let-values ([....]) ....), (letrec-values ([....]) ....),
    ;; (letrec-syntaxes+values ([....]) ....), etc.
    [pattern (stuff ... next)
             #:with :mod/acc-sig #'next]))

(define-syntax-parser mod/acc
  [(_ [sig-entry/rev ...] [val-id/rev ...])
   #:with [sig-entry ...] (reverse (attribute sig-entry/rev))
   #:with [val-id ...] (reverse (attribute val-id/rev))
   #:with s:sig #'(sig:sig sig-entry ...)

   #:with [[sym/id ...] ...] #'[['val-id val-id] ...]
   #:with final-expression #'(let-values ([() s.residual])
                               (hash sym/id ... ...))

   (syntax-property #'final-expression
     mod/acc-sig-prop
     (syntax-local-introduce
      (attribute s.expansion)))]

  [(head [ent/rev ...] [v/rev ...] defn rest-defn ...)
   #:with defn- (local-expand #'defn 'module mod-stop-ids)
   (syntax-parse #'defn-
     #:literal-sets [mod-stop-literals]

     [(begin form ...)
      #'(mod/acc [ent/rev ...] [v/rev ...] form ... rest-defn ...)]

     [d:hackett-module-component
      (syntax-track-origin
       #'(begin
           defn-
           (mod/acc [d.sig-entry ent/rev ...] [d.val-id ... v/rev ...]
                    rest-defn ...))
       #'d.residual
       #'head)]

     [:pass-through
      #'(begin defn- (mod/acc [ent/rev ...] [v/rev ...] rest-defn ...))]

     [{~and (head . _) :disallowed}
      (raise-syntax-error #f
        (format "~a form not allowed inside of a module"
                #'head)
        #'defn)])])

(define-syntax-parser mod
  [(_ defn ...)
   ;; put the defns in a new scope
   #:with [defn* ...] ((make-syntax-introducer #true) #'[defn ...])

   ;; expand `mod/acc` in a (let () ....) to parse the definitions
   #:with expansion:mod/acc-sig
   (local-expand #'(let ()
                     (mod/acc [] [] defn* ...))
                 'module
                 '())

   (attach-sig #'expansion
               (attribute expansion.sig))])
