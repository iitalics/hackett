#lang racket/base
(provide
 attach-sig
 sig⇒
 generate-module-var-bindings)

(require
 syntax/parse
 hackett/private/util/stx
 (for-template "../rep/sig.rkt"))

(define (attach-sig stx s)
  (syntax-property stx 'sig: s))

(define (sig⇒ stx [ctx #f])
  (define m-
    (local-expand stx 'module-begin '() ctx))
  (define sig
    (syntax-local-introduce
     (syntax-property m- 'sig:)))
  (list m- sig))

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
    (generate-temporaries opaque-type-syms))

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
