#lang racket/base
(provide
 (struct-out module-var-transformer)
 generate-module-var-bindings
 syntax-local-bind-module
 module-binding)

(require
 racket/list
 racket/match
 racket/syntax
 syntax/id-table
 syntax/parse
 (only-in syntax/parse [attribute @])
 syntax/parse/class/local-value
 hackett/private/util/stx
 "expand-check-prop.rkt"
 "../prop-reintroducible-dot-type.rkt"
 "../prop-dot-accessible.rkt"
 "../util/stx.rkt"
 "../util/hash.rkt"
 (for-template "../rep/sig-literals.rkt"
               (only-in racket/base #%app quote)
               (prefix-in l: "../link/mod.rkt"))
 (for-template hackett/private/type-language
               (only-in hackett/private/adt
                        data-constructor
                        [type-constructor data-type-constructor])))

;; internal-id : Id
;; signature : Signature
;; type-ids : [Hash Symbol Id]
;; value-ids : [Hash Symbol Id]
;; pattern-ids : [Hash Symbol Id]
;; submod-ids : [Hash Symbol Id]
(struct module-var-transformer
  [internal-id
   signature
   type-ids
   value-ids
   pattern-ids
   submod-ids]
  #:property prop:procedure
  (λ (self stx)
    (define x- (module-var-transformer-internal-id self))
    (define sig (module-var-transformer-signature self))
    ((make-variable-like-transformer
      (λ (id)
        (attach-sig (replace-stx-loc x- id) sig)))
     stx))
  #:property prop:dot-accessible
  (λ (self)
    (match-define (module-var-transformer id _ ht hv hp hm) self)
    (dot-accessible
     id
     (λ (val-key) (hash-ref hv val-key #f))
     (λ (pat-key) (hash-ref hp pat-key #f))
     (λ (type-key) (hash-ref ht type-key #f)))))

(struct opaque-type-constructor
  [module-id external-sym]
  #:property prop:reintroducible-dot-type
  (λ (self)
    (reintroducible-dot-type
     (opaque-type-constructor-module-id self)
     (opaque-type-constructor-external-sym self)))
  #:property prop:procedure
  (λ (self stx)
    ((make-variable-like-transformer
      (λ (id) #`(#%type:con #,id)))
     stx)))

(struct data-type-constructor/reintroducible
  data-type-constructor
  [module-id external-sym]
  #:property prop:reintroducible-dot-type
  (λ (self)
    (reintroducible-dot-type
     (data-type-constructor/reintroducible-module-id self)
     (data-type-constructor/reintroducible-external-sym self)))
  #:property prop:procedure
  (λ (self stx)
    ((make-variable-like-transformer
      (λ (id) #`(#%type:con #,id)))
     stx)))

;; Generates bindings needed to introduce a module with the
;; given name and signature.
;; Id Id Signature ->
;;   [List [Listof [List Id TransformerStx]]     ; transformer bindings
;;         [Listof [List Id Stx]]]               ; value binding
(define (generate-module-var-bindings m-binding-id
                                      m-internal-id
                                      signature)

  ;;================
  ;; TODO:
  ;;   - opaque types
  ;;   - type aliases
  ;;   - data types
  ;;   - data constructors (values + patterns)
  ;;   - values
  ;;   - submodules ?

  (define-values [sig-internal-ids sig-only-decls]
    (syntax-parse signature
      #:literal-sets [sig-literals]
      [(#%sig ii:hash-literal
              ds:hash-literal)
       (values (@ ii.value) (@ ds.value))]
      [(#%pi-sig . _)
       (values (hash) (hash))]))

  ;; these temporaries are for syntax bindings
  (define key->new-id
    (for/hash ([(key decl) (in-hash sig-only-decls)])
      (define prefix
        (cond
          ;; NOTE: this is just for debugging
          [(decl-type-opaque? decl) "opaque"]
          [(decl-type-data? decl)   "data"]
          [(decl-type? decl)        "alias"]
          [(decl-constructor? decl) "ctor"]
          [(decl-val? decl)         "val"]
          [(decl-module? decl)
           (error "module binding unsupported")]))
      (define id
        (generate-temporary
         (format-symbol "~a:~a" prefix (namespaced-symbol key))))
      (values key id)))

  (define new-sig-decls
    (signature-substs
     sig-only-decls
     (for/fold ([tbl (make-immutable-free-id-table)])
               ([(key id) (in-hash sig-internal-ids)])
       (free-id-table-set tbl id (hash-ref key->new-id key)))))

  ;; [Listof Key] {[Listof Key] [Listof Id]} -> Stx
  ;; creates syntax to construct a hash from keys to ids. the keys in the
  ;; first argument are mapped to the new id (key->new-id)
  (define (make-hash-syntax keys [also-keys '()] [also-ids '()])
    (define/syntax-parse [key ...] (append keys also-keys))
    (define/syntax-parse [id ...]
      (append (map (λ (k) (hash-ref key->new-id k)) keys)
              also-ids))
    (define/syntax-parse [[key/id ...] ...] #'[['key (quote-syntax id)] ...])
    #'(hash key/id ... ...))

  ;; extract sets of keys from later processing
  (define type-keys (matching-decl-keys signature decl-type?))
  (define ctor-keys (matching-decl-keys signature decl-constructor?))
  (define val-keys (matching-decl-keys signature decl-val?))

  ;; these temporaries are for value bindings
  (define constructor-val-ids (generate-temporaries ctor-keys))
  (define constructor-pat-ids (generate-temporaries ctor-keys))
  (define value-val-ids (generate-temporaries val-keys))

  ;; generate bindings for types

  (define type-bindings
    (for/list ([key (in-list type-keys)])
      (define id (hash-ref key->new-id key))
      (define rhs
        (syntax-parse (hash-ref new-sig-decls key)
          #:literal-sets [sig-literals]

          [(#%type-decl (#%alias eq-type))
           #'(make-variable-like-transformer (quote-syntax eq-type))]

          [(#%type-decl (#%opaque))
           #`(opaque-type-constructor
              (quote-syntax #,m-internal-id)
              '#,(namespaced-symbol key))]

          [(#%type-decl (#%data c ...))
           ;; find the "new" constructor ids for the module being introduced
           (define/syntax-parse
             [c-new-id ...]
             (for/list ([c-sig-id (in-list (@ c))])
               (or (for/first ([(key id) (in-hash sig-internal-ids)]
                               #:when (free-identifier=? c-sig-id id))
                     (hash-ref key->new-id key))
                   (raise-syntax-error c-sig-id
                     "constructor declaration not found in signature"))))

           ;; TODO: get the actual type-var arity
           (define type-var-arity 0)

           #`(data-type-constructor/reintroducible
              (quote-syntax (#%type:con #,id))     ; type
              '#,type-var-arity                    ; arity
              (list (quote-syntax c-new-id) ...)   ; constructor ids
              #f                                   ; fixity
              (quote-syntax #,m-internal-id)       ; module id
              '#,(namespaced-symbol key))]))       ; external key

      (list id rhs)))

  ;; generate data-constructor bindings for data types

  (define constructor-val-bindings
    (for/list ([key (in-list ctor-keys)]
               [val-id (in-list constructor-val-ids)])
      (list val-id #`(l:mod-value-ref #,m-internal-id '#,(namespaced-symbol key)))))

  (define constructor-pat-bindings
    (for/list ([key (in-list ctor-keys)]
               [pat-id (in-list constructor-pat-ids)])
      (list pat-id #`(l:mod-pattern-ref #,m-internal-id '#,(namespaced-symbol key)))))

  (define constructor-bindings
    (for/list ([key (in-list ctor-keys)]
               [pat-id (in-list constructor-pat-ids)]
               [val-id (in-list constructor-val-ids)])
      (define id (hash-ref key->new-id key))
      (define decl (hash-ref new-sig-decls key))
      (define/syntax-parse [pat-id* val-id* sym*]
        (list pat-id val-id (namespaced-symbol key)))

      (define/syntax-parse ({~literal #%constructor-decl} t) decl)

      (list id
            #'(data-constructor
               (make-variable-like-transformer (quote-syntax val-id*))
               (quote-syntax t)
               (λ (sub-pats) #`(l:app/pat-info pat-id* #,sub-pats))
               #f))))

  ;; generate mod-value-ref expressions for values

  (define val-bindings
    (for/list ([key (in-list val-keys)]
               [val-id (in-list value-val-ids)])
      (list val-id
            #`(l:mod-value-ref #,m-internal-id '#,(namespaced-symbol key)))))

  (define typed-val-bindings
    (for/list ([key (in-list val-keys)]
               [val-id (in-list value-val-ids)])
      (define id (hash-ref key->new-id key))
      (define decl (hash-ref new-sig-decls key))
      (define/syntax-parse ({~literal #%val-decl} t) decl)
      (list id
            #'(typed ?????))))

  ;; generate module-var-transformer binding for module name

  (define module-binding
    (list m-binding-id
          #`(module-var-transformer
             (quote-syntax #,m-internal-id)
             (quote-syntax #,signature)
             #,(make-hash-syntax type-keys)   ; types
             #,(make-hash-syntax (append val-keys ctor-keys)) ; values
             #,(make-hash-syntax ctor-keys)   ; patterns
             (hash))))                        ; submods

  ;; ------

  (define all-syntax-bindings
    (cons module-binding
          (append type-bindings
                  constructor-bindings
                  typed-val-bindings)))

  (define all-value-bindings
    (append constructor-val-bindings
            constructor-pat-bindings
            val-bindings))

  (list all-syntax-bindings
        all-value-bindings))


;; Id Id Signature IntDefCtx -> [Listof [List Id Expr-Stx]]
;; adds bindings introduced by the module to the given intdef-ctx. returns
;; an association list of the value bindings that should be introduced along
;; with the module.
;;
;; ASSUME: signature must be expanded. internal-id should be
;; already bound in the intdef-ctx.
(define (syntax-local-bind-module name internal-id signature intdef-ctx)
  ;; NOTE: we ignore the RHS of the value bindings,
  ;;   since this is used for local-expanding, not evalutating.
  (match-define (list ids/transformers ids/exprs)
    (generate-module-var-bindings name internal-id signature))
  (define/syntax-parse [([stx-id transformer] ...)
                        ([val-id expr] ...)]
    (list ids/transformers ids/exprs))
  (syntax-local-bind-syntaxes (@ val-id) #f intdef-ctx)
  (syntax-local-bind-syntaxes (@ stx-id) #`(values transformer ...) intdef-ctx)
  (for/list ([id/expr (in-list ids/exprs)])
    (list (internal-definition-context-introduce intdef-ctx (first id/expr))
          (internal-definition-context-introduce intdef-ctx (second id/expr)))))

(define-syntax-class module-binding
  #:description "module name"
  #:attributes [value internal-id sig type-ids value-ids pattern-ids submod-ids]
  [pattern {~var m (local-value module-var-transformer?)}
           #:attr value (@ m.local-value)
           #:do [(match-define (module-var-transformer x-
                                                       s
                                                       type-key->id
                                                       val-key->id
                                                       pat-key->id
                                                       submod-key->id)
                   (@ value))]
           #:with internal-id (syntax-local-introduce x-)
           #:attr sig (syntax-local-introduce s)
           #:attr type-ids type-key->id
           #:attr value-ids val-key->id
           #:attr pattern-ids pat-key->id
           #:attr submod-ids submod-key->id])

;; ---------------------------------------------------------

;; Signature [Decl -> Bool] -> [Listof Key]
(define (matching-decl-keys s decl-matches?)
  (syntax-parse s
    #:literal-sets [sig-literals]
    [(#%pi-sig . _) '()]
    [(#%sig . _)
     (for/list ([(key decl) (in-hash (sig-decls s))]
                #:when (decl-matches? decl))
       key)]))

;; SymStr [Listof Key] -> [Listof Id]
#;
(define (generate-prefixed-temporaries prefix keys)
  (generate-temporaries
   (for/list ([k (in-list keys)])
     (format-symbol "~a~a" prefix (namespaced-symbol k)))))
