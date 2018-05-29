#lang racket/base
(provide
 (struct-out module-var-transformer)
 (struct-out opaque-type-constructor)
 generate-module-var-bindings
 syntax-local-bind-module
 module-binding)

(require
 racket/list
 racket/match
 racket/syntax
 syntax/parse
 (only-in syntax/parse [attribute @])
 syntax/parse/class/local-value
 hackett/private/util/stx
 "expand-check-prop.rkt"
 "../util/stx.rkt"
 (for-template "../rep/sig-literals.rkt")
 (for-template hackett/private/type-language
               (only-in hackett/private/adt data-constructor)))

;; internal-id : Id
;; signature : Signature
;; opaque-type-ids : [Hash Symbol Id]
;; constructor-ids : [Hash Symbol Id]
(struct module-var-transformer
  [internal-id
   signature
   opaque-type-ids
   constructor-ids]
  #:property prop:procedure
  (λ (self stx)
    (define x- (module-var-transformer-internal-id self))
    (define sig (module-var-transformer-signature self))
    ((make-variable-like-transformer
      (λ (id)
        (attach-sig (replace-stx-loc x- id) sig)))
     stx)))

(struct opaque-type-constructor
  [module-internal-id
   external-sym])

;; Generates bindings needed to introduce a module with the
;; given name and signature.
;; Id Id Signature ->
;;   [List [Listof [List Id TransformerStx]]     ; transformer bindings
;;         [Listof [List Id Stx]]]               ; value binding
(define (generate-module-var-bindings name internal-id s)

  ; generate #%type:con's for opaque types

  (define opaque-type-syms
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(#%pi-sig . _) '()]
      [(#%sig . _)
       (for/list ([(sym decl) (in-hash (sig-decls s))]
                  #:when (decl-type-opaque? decl))
         sym)]))

  (define opaque-type-ids
    (generate-temporaries
     (for/list ([s (in-list opaque-type-syms)])
       (format-symbol "opaque:~a.~a" (syntax-e name) s))))

  (define opaque-type-bindings
    (for/list ([sym (in-list opaque-type-syms)]
               [id (in-list opaque-type-ids)])
      (list id
            #`(opaque-type-constructor
               (quote-syntax #,internal-id)
               '#,sym))))

  (define/syntax-parse [op-sym ...] opaque-type-syms)
  (define/syntax-parse [op-id ...] opaque-type-ids)
  (define/syntax-parse [[op-sym/id ...] ...] #'[['op-sym (quote-syntax op-id)] ...])

  (define type-expansion-ctx
    (module-make-type-expansion-context
     s
     (make-immutable-hash (map cons opaque-type-syms opaque-type-ids))))

  ; generate data-constructor bindings for data types

  (define constructor-syms
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(#%pi-sig . _) '()]
      [(#%sig . _)
       (for/list ([(sym decl) (in-hash (sig-decls s))]
                  #:when (decl-constructor? decl))
         sym)]))

  (define constructor-ids
    (generate-temporaries
     (for/list ([s (in-list constructor-syms)])
       (format-symbol "ctor:~a.~a" (syntax-e name) s))))

  (define constructor-bindings
    (for/list ([sym (in-list constructor-syms)]
               [id (in-list constructor-ids)])
      (define/syntax-parse
        ({~literal #%constructor-decl} {~var t (type type-expansion-ctx)})
        (hash-ref (sig-decls s) sym))

      (list id
            ;; ========= TODO: finish implementing this ==========
            #'(data-constructor
               (make-variable-like-transformer #'???)
               (quote-syntax t.expansion)
               (λ (sub-pats)
                 #`(app/pat-info
                    ???
                    #,@sub-pats))
               #f))))

  (define/syntax-parse [ctor-sym ...] constructor-syms)
  (define/syntax-parse [ctor-id ...] constructor-ids)
  (define/syntax-parse [[ctor-sym/id ...] ...] #'[['ctor-sym (quote-syntax ctor-id)] ...])

  ; generate module-var-transformer binding for module name

  (define module-binding
    (list name
          #`(module-var-transformer
             (quote-syntax #,internal-id)
             (quote-syntax #,s)
             (hash op-sym/id ... ...)
             (hash ctor-sym/id ... ...))))

  (define all-syntax-bindings
    (cons module-binding
          (append opaque-type-bindings
                  constructor-bindings)))

  (list all-syntax-bindings
        '()))

;; Id Id Signature IntDefCtx -> Void
;; signature must be expanded. ASSUME: internal-id should be
;; already bound in the intdef-ctx.
(define (syntax-local-bind-module name internal-id signature intdef-ctx)
  ;; NOTE: we ignore the RHS of the value bindings,
  ;;   since this is used for local-expanding, not evalutating.
  (define/syntax-parse [([stx-id transformer] ...)
                        ([val-id _] ...)]
    (generate-module-var-bindings name internal-id signature))
  (syntax-local-bind-syntaxes (@ val-id) #f intdef-ctx)
  (syntax-local-bind-syntaxes (@ stx-id) #`(values transformer ...) intdef-ctx))

;; Signature [Hash Symbol Id] -> IntDefCtx
(define (module-make-type-expansion-context sig opaque-sym->id)
  (syntax-parse sig
    #:literal-sets [sig-literals]
    [(#%sig internal-ids:hash-literal
            decls:hash-literal)
     (define intdef-ctx
       (syntax-local-make-definition-context))

     ;; create type transformers for #%type-decl:
     ;;  - #%alias maps to the rhs
     ;;  - #%opaque maps to (#%type:con <opaque-id>)

     (for ([(sym decl) (in-hash (@ decls.value))])
       (define internal-id (hash-ref (@ internal-ids.value) sym))
       (syntax-parse decl
         #:literal-sets [sig-literals]

         [(#%type-decl (#%alias rhs))
          (syntax-local-bind-syntaxes
           (list internal-id)
           #'(make-variable-like-transformer
              (quote-syntax rhs))
           intdef-ctx)]

         [(#%type-decl (#%opaque))
          #:with type-con-id (hash-ref opaque-sym->id sym)
          (syntax-local-bind-syntaxes
           (list internal-id)
           #'(make-variable-like-transformer
              (quote-syntax (#%type:con type-con-id)))
           intdef-ctx)]

         [_ (void)]))

     intdef-ctx]

    [(#%pi-sig . _)
     (syntax-local-make-definition-context)]))


(define-syntax-class module-binding
  #:description "module name"
  #:attributes [value internal-id sig opaque-ids constructor-ids expansion-ctx]
  [pattern {~var m (local-value module-var-transformer?)}
           #:attr value (@ m.local-value)
           #:do [(match-define (module-var-transformer x- s op-sym->id ctor-sym->id)
                   (@ value))]
           #:with internal-id (syntax-local-introduce x-)
           #:attr sig (syntax-local-introduce s)
           #:attr opaque-ids op-sym->id
           #:attr constructor-ids ctor-sym->id
           #:attr expansion-ctx
           (module-make-type-expansion-context (@ sig) (@ opaque-ids))])
