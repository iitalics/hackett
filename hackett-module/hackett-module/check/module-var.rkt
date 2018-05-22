#lang racket/base
(provide
 (struct-out module-var-transformer)
 (struct-out opaque-type-constructor)
 generate-module-var-bindings
 module-binding)

(require
 racket/match
 racket/syntax
 syntax/parse
 (only-in syntax/parse [attribute @])
 syntax/parse/class/local-value
 hackett/private/util/stx
 "expand-check.rkt"
 "../util/stx.rkt"
 (for-template "../rep/sig.rkt")
 (for-template hackett/private/type-language))

;; internal-id : Id
;; signature : Signature
;; opaque-type-ids : [Hash Symbol Id]
(struct module-var-transformer
  [internal-id
   signature
   opaque-type-ids]
  #:property prop:procedure
  (λ (self stx)
    (define x- (module-var-transformer-internal-id self))
    (define sig (module-var-transformer-signature self))
    ((make-variable-like-transformer
      (λ (id)
        (attach-sig (replace-stx-loc x- id) sig)))
     stx)))

(struct opaque-type-constructor
  [module-id
   external-sym])

;; Generates bindings needed to introduce a module with the
;; given name and signature.
;; Id Id Signature -> [Listof [List Id TransformerStx]]
(define (generate-module-var-bindings name internal-id s)
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
               (quote-syntax #,name)
               '#,sym))))

  (define/syntax-parse [sym ...] opaque-type-syms)
  (define/syntax-parse [id ...] opaque-type-ids)
  (define/syntax-parse [[sym/id ...] ...] #'[['sym (quote-syntax id)] ...])

  (define module-binding
    (list name
          #`(module-var-transformer
             (quote-syntax #,internal-id)
             (quote-syntax #,s)
             (hash sym/id ... ...))))

  (cons module-binding opaque-type-bindings))


;; Sig [Hash Symbol Id] -> IntDefCtx
;; note: the sig argument must be a sig (e.g. not a Π-sig)
(define (module-make-type-expansion-context sig sym->id)
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
          #:with type-con-id (hash-ref sym->id sym)
          (syntax-local-bind-syntaxes
           (list internal-id)
           #'(make-variable-like-transformer
              (quote-syntax (#%type:con type-con-id)))
           intdef-ctx)]

         [_ (void)]))

     intdef-ctx]))


(define-syntax-class module-binding
  #:description "module name"
  #:attributes [value internal-id sig opaque-ids expansion-ctx]
  [pattern {~var m (local-value module-var-transformer?)}
           #:attr value (@ m.local-value)
           #:do [(match-define (module-var-transformer x- s sym->id)
                   (@ value))]
           #:with internal-id (syntax-local-introduce x-)
           #:attr sig (syntax-local-introduce s)
           #:attr opaque-ids sym->id
           #:attr expansion-ctx
           (module-make-type-expansion-context (@ sig) (@ opaque-ids))])
