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
;; opaque-type-ids : [Hash Symbol Id]
;; data-type-ids : [Hash Symbol Id]
;; constructor-ids : [Hash Symbol Id]
(struct module-var-transformer
  [internal-id
   signature
   opaque-type-ids
   data-type-ids
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
    (match-define (module-var-transformer _ _ ho hd hv hp hm) self)
    (dot-accessible
     (λ (val-key) (hash-ref hv val-key #f))
     (λ (pat-key) (hash-ref hp pat-key #f))
     (λ (type-key) (or (hash-ref ho type-key #f)
                       (hash-ref hd type-key #f))))))

(struct opaque-type-constructor
  [module-id external-sym]
  #:property prop:reintroducible-dot-type
  (λ (self)
    (reintroducible-dot-type
     (opaque-type-constructor-module-id self)
     (opaque-type-constructor-external-sym self))))

(struct data-type-constructor/reintroducible
  data-type-constructor
  [module-id external-sym]
  #:property prop:reintroducible-dot-type
  (λ (self)
    (reintroducible-dot-type
     (data-type-constructor/reintroducible-module-id self)
     (data-type-constructor/reintroducible-external-sym self))))

;; Generates bindings needed to introduce a module with the
;; given name and signature.
;; Id Id Signature ->
;;   [List [Listof [List Id TransformerStx]]     ; transformer bindings
;;         [Listof [List Id Stx]]]               ; value binding
(define (generate-module-var-bindings name internal-id s)

  (define opaque-type-keys
    (matching-decl-keys s decl-type-opaque?))

  (define opaque-type-ids
    (generate-prefixed-temporaries (format-symbol "opaque:~a." name)
                                   opaque-type-keys))

  (define data-type-keys
    (matching-decl-keys s decl-type-data?))

  (define data-type-ids
    (generate-prefixed-temporaries (format-symbol "data:~a." name)
                                   data-type-keys))

  (define constructor-keys
    (matching-decl-keys s decl-constructor?))

  (define constructor-ids
    (generate-prefixed-temporaries (format-symbol "ctor:~a." name)
                                   constructor-keys))

  (define constructor-val-ids (generate-temporaries constructor-keys))
  (define constructor-pat-ids (generate-temporaries constructor-keys))

  (define constructor-key->id
    (hash-zip constructor-keys constructor-ids))

  (define value-keys
    (matching-decl-keys s decl-val?))

  (define value-ids (generate-temporaries value-keys))

  ;; make intdef context for expanding types

  (define type-expansion-ctx
    (module-make-type-expansion-context
     s
     (hash-zip opaque-type-keys opaque-type-ids)
     (hash-zip data-type-keys data-type-ids)))

  ;; generate #%type:con's for opaque types

  (define opaque-type-bindings
    (for/list ([key (in-list opaque-type-keys)]
               [id (in-list opaque-type-ids)])
      (list id
            #`(opaque-type-constructor
               (quote-syntax #,internal-id)
               '#,(namespaced-symbol key)))))

  (define/syntax-parse [op-key ...] opaque-type-keys)
  (define/syntax-parse [op-id ...] opaque-type-ids)
  (define/syntax-parse [[op-key/id ...] ...] #'[['op-key (quote-syntax op-id)] ...])

  ;; generate #%type:con's for data types

  (define data-type-bindings
    (for/list ([key (in-list data-type-keys)]
               [id (in-list data-type-ids)])
      ;; get the constructors
      (define/syntax-parse
        ({~literal #%type-decl} ({~literal #%data} c ...))
        (hash-ref (sig-decls s) key))

      ;; find the "new" constructor ids for the module being introduced
      (define/syntax-parse
        [c-binding-id ...]
        (for/list ([c-id (in-list (@ c))])
          (or (for/first ([(key id) (in-hash (sig-internal-ids s))]
                          #:when (free-identifier=? c-id id))
                (hash-ref constructor-key->id key))
              (raise-syntax-error c-id
                "constructor declaration not found in signature"))))

      ;; TODO: get the actual type-var arity
      (define type-var-arity 0)

      (list id
            #`(data-type-constructor/reintroducible
               (quote-syntax (#%type:con #,id))        ; type
               '#,type-var-arity                       ; arity
               (list (quote-syntax c-binding-id) ...)  ; constructor ids
               #f                                      ; fixity
               (quote-syntax #,internal-id)            ; module id
               '#,(namespaced-symbol key)              ; external key
               ))))

  (define/syntax-parse [data-key ...] data-type-keys)
  (define/syntax-parse [data-id ...] data-type-ids)
  (define/syntax-parse [[data-key/id ...] ...] #'[['data-key (quote-syntax data-id)] ...])

  ;; generate data-constructor bindings for data types

  (define constructor-val-bindings
    (for/list ([key (in-list constructor-keys)]
               [id (in-list constructor-val-ids)])
      (list id #`(l:mod-value-ref #,internal-id '#,(namespaced-symbol key)))))

  (define constructor-pat-bindings
    (for/list ([key (in-list constructor-keys)]
               [id (in-list constructor-pat-ids)])
      (list id #`(l:mod-pattern-ref #,internal-id '#,(namespaced-symbol key)))))

  (define constructor-bindings
    (for/list ([key (in-list constructor-keys)]
               [id (in-list constructor-ids)]
               [pat-id (in-list constructor-pat-ids)]
               [val-id (in-list constructor-val-ids)])
      (define/syntax-parse [m-id* sym* pat-id* val-id*]
        (list internal-id (namespaced-symbol key) pat-id val-id))
      (define/syntax-parse
        ({~literal #%constructor-decl} {~var t (type type-expansion-ctx)})
        (hash-ref (sig-decls s) key))

      (list id
            #'(data-constructor
               (make-variable-like-transformer (quote-syntax val-id*))
               (quote-syntax t.expansion)
               (λ (sub-pats) #`(l:app/pat-info pat-id* #,sub-pats))
               #f))))

  (define/syntax-parse [ctor-key ...] constructor-keys)
  (define/syntax-parse [ctor-id ...] constructor-ids)
  (define/syntax-parse [[ctor-key/id ...] ...] #'[['ctor-key (quote-syntax ctor-id)] ...])

  ;; generate mod-value-ref expressions for values

  (define value-bindings
    (for/list ([key (in-list value-keys)]
               [id (in-list value-ids)])
      (list id #`(l:mod-value-ref #,internal-id '#,(namespaced-symbol key)))))

  (define/syntax-parse [val-key ...] value-keys)
  (define/syntax-parse [val-id ...] value-ids)
  (define/syntax-parse [[val-key/id ...] ...] #'[['val-key (quote-syntax val-id)] ...])

  ;; generate module-var-transformer binding for module name

  (define module-binding
    (list name
          #`(module-var-transformer
             (quote-syntax #,internal-id)
             (quote-syntax #,s)
             (hash op-key/id ... ...)          ; opaque types
             (hash data-key/id ... ...)        ; data types
             (hash ctor-key/id ... ... val-key/id ... ...) ; values
             (hash ctor-key/id ... ...)        ; patterns
             (hash))))                         ; submods

  ;; ------

  (define all-syntax-bindings
    (cons module-binding
          (append opaque-type-bindings
                  data-type-bindings
                  constructor-bindings)))

  (define all-value-bindings
    (append constructor-val-bindings
            constructor-pat-bindings
            value-bindings))

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

;; Signature [Hash Key Id] [Hash Key Id] -> IntDefCtx
(define (module-make-type-expansion-context sig opaque-key->id data-key->id)
  (syntax-parse sig
    #:literal-sets [sig-literals]
    [(#%sig internal-ids:hash-literal
            decls:hash-literal)
     (define intdef-ctx
       (syntax-local-make-definition-context))

     ;; create type transformers for #%type-decl:
     ;;  - #%alias maps to the rhs
     ;;  - #%opaque maps to (#%type:con <opaque-id>)

     (for ([(key decl) (in-hash (@ decls.value))])
       (define internal-id (hash-ref (@ internal-ids.value) key))
       (syntax-parse decl
         #:literal-sets [sig-literals]

         [(#%type-decl (#%alias rhs))
          (syntax-local-bind-syntaxes
           (list internal-id)
           #'(make-variable-like-transformer
              (quote-syntax rhs))
           intdef-ctx)]

         [(#%type-decl (#%opaque))
          #:with type-con-id (hash-ref opaque-key->id key)
          (syntax-local-bind-syntaxes
           (list internal-id)
           #'(make-variable-like-transformer
              (quote-syntax (#%type:con type-con-id)))
           intdef-ctx)]

         [(#%type-decl (#%data . _))
          #:with type-con-id (hash-ref data-key->id key)
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
  #:attributes [value internal-id sig opaque-ids value-ids pattern-ids submod-ids expansion-ctx]
  [pattern {~var m (local-value module-var-transformer?)}
           #:attr value (@ m.local-value)
           #:do [(match-define (module-var-transformer x-
                                                       s
                                                       op-key->id
                                                       data-key->id
                                                       val-key->id
                                                       pat-key->id
                                                       submod-key->id)
                   (@ value))]
           #:with internal-id (syntax-local-introduce x-)
           #:attr sig (syntax-local-introduce s)
           #:attr opaque-ids op-key->id
           #:attr data-ids data-key->id
           #:attr value-ids val-key->id
           #:attr pattern-ids pat-key->id
           #:attr submod-ids submod-key->id
           #:attr expansion-ctx
           (module-make-type-expansion-context (@ sig)
                                               (@ opaque-ids)
                                               (@ data-ids)
                                               ; TOOD: submod-ids?
                                               )])

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
(define (generate-prefixed-temporaries prefix keys)
  (generate-temporaries
   (for/list ([k (in-list keys)])
     (format-symbol "~a~a" prefix (namespaced-symbol k)))))
