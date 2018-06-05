#lang racket/base

(provide sig
         decl
         expand-sig
         expand-decl
         signature-subst
         signature-substs
         sig-where)

(require racket/syntax
         racket/list
         syntax/parse
         syntax/parse/define
         (only-in syntax/parse [attribute @])
         syntax/intdef
         syntax/id-table
         threading
         hackett/private/util/stx
         hackett/private/typecheck
         "../util/stx.rkt"
         "../util/stx-traverse.rkt"
         "../util/hash.rkt"
         "../check/module-var.rkt"
         (for-syntax racket/base)
         (for-template racket/base
                       "../dot.rkt"
                       "sig-literals.rkt"))

; helper for expanding signatures; combines "residual" properties
(define (residual origs id)
  (for/fold ([acc #'(values)])
            ([orig (in-list (syntax->list origs))])
    (syntax-track-origin acc orig id)))

;; ---------------------------------------------

(define ((make-local-expand literal-ids) stx [intdef-ctx #f])
  (local-expand stx 'expression literal-ids intdef-ctx))

(define-simple-macro
  (define-expansion-class name:id desc:str expanded-name:id expand-fn:expr)
  (define-syntax-class (name [intdef-ctx #f])
    #:description desc
    #:attributes [expansion residual]
    [pattern stx
             #:with {~var || (expanded-name intdef-ctx)}
             (expand-fn #'stx intdef-ctx)]))

(define sig-local-expand
  (make-local-expand sig-literal-ids))

(define-expansion-class ref-id "id reference" expanded-id sig-local-expand)

(define-expansion-class partial-sig "signature" partial-expanded-sig sig-local-expand)
(define-expansion-class partial-decl "signature declaration" partial-expanded-decl sig-local-expand)

;; Stx [IntDefCtx] -> PartialSig
(define (partial-expand-sig stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var S (partial-sig intdef-ctx)}
     #'S.expansion]))

;; Stx [IntDefCtx] -> PartialDecl
(define (partial-expand-decl stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var D (partial-decl intdef-ctx)}
     #'D.expansion]))

(define-expansion-class sig "signature" expanded-sig partial-expand-sig)
(define-expansion-class decl "signature declaration" expanded-decl partial-expand-decl)

;; Stx [IntDefCtx] -> Signature
(define (expand-sig stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var S (sig intdef-ctx)}
     #'S.expansion]))

;; Stx [IntDefCtx] -> Decl
(define (expand-decl stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var D (decl intdef-ctx)}
     #'D.expansion]))

;; ---------------------------------------------

;; Signature [FreeIdTbl Id] -> SignatureStx
;; note: result is stx, aka. unexpanded
(define (signature-substs s mapping)
  (let traverse ([stx s])
    (syntax-parse stx
      [:id
       (free-id-table-ref mapping stx stx)]
      [_
       (traverse-stx/recur stx traverse)])))

;; Signature Id Id -> Signature
(define (signature-subst s x-old x-new)
  (define mapping
    (make-immutable-free-id-table
     (list (cons x-old x-new))))
  (signature-substs s mapping))

;; ---------------------------------------------

;; declares that the decl exists without binding what
;; its "equal to".
(define (syntax-local-declare-decl id decl intdef-ctx)
  (syntax-parse decl
    #:context 'syntax-local-declare-decl
    #:literal-sets [sig-literals]
    [{~or (#%type-decl . _)
          (#%val-decl . _)
          (#%constructor-decl . _)}
     (syntax-local-bind-syntaxes (list id) #f intdef-ctx)]

    [(#%module-decl (#%pi-sig . _))
     (syntax-local-bind-syntaxes (list id) #f intdef-ctx)]

    [(#%module-decl (#%sig internal-ids:hash-literal
                           decls:hash-literal))

     (define/syntax-parse [[key tmp-id] ...]
       (for/list ([(key id) (in-hash (@ internal-ids.value))])
         (define decl (hash-ref (@ decls.value) key))
         (define tmp-id (generate-temporary (namespaced-symbol key)))
         (syntax-local-declare-decl tmp-id decl intdef-ctx)
         (list key tmp-id)))

     (define/syntax-parse [[key/tmp-id ...] ...] #'[['key (quote-syntax tmp-id)] ...])

     (syntax-local-bind-syntaxes
      (list id)
      #'(declared-module-var
         (hash key/tmp-id ... ...))
      intdef-ctx)]))

(struct declared-module-var [key->tmp-id])

;; -----------------

(define-syntax-class (expanded-id intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  [pattern expansion:id
           #:with residual (residual #'[] #'expansion)])

;; expects result from partial-sig.expansion as input, outputs
;; a fully expanded sig in .expansion.
(define-syntax-class (expanded-sig intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  ;; A Key is a (namespaced Namespace Symbol)
  ;; (#%sig
  ;;   #:hash([key . internal-id]
  ;;          ...)
  ;;   #:hash([key . decl]
  ;;          ...))
  [pattern (head:#%sig
            internal-ids:hash-literal
            decls:hash-literal)

           ;; create a context where all of the internal-ids are bound
           #:do [(define intdef-ctx* (syntax-local-make-definition-context intdef-ctx))
                 (define (intro stx)
                   (internal-definition-context-introduce intdef-ctx* stx))

                 (for ([(key id) (in-hash (@ internal-ids.value))])
                   (define decl (hash-ref (@ decls.value) key))
                   (syntax-local-declare-decl id decl intdef-ctx*))]

           #:with internal-ids- (intro #'internal-ids)
           #:with [{~var decl- (expanded-decl intdef-ctx*)} ...] (attribute decls.values)
           #:with decls-expansion-
           (hash-zip (attribute decls.keys) (attribute decl-.expansion))
           #:attr expansion (~>> (syntax/loc/props this-syntax
                                                   (head internal-ids-
                                                         decls-expansion-))
                                 (internal-definition-context-track intdef-ctx*))
           #:attr residual (residual #'[decl-.residual ... expansion]
                                     #'head)]

  ;; (#%pi-sig ([param-id sig]) sig)
  [pattern (head:#%pi-sig ([x:id {~var A (sig intdef-ctx)}]) B:expr)
           #:with x- (generate-temporary #'x)

           ;; create a context where x is bound
           #:do [(define intdef-ctx* (syntax-local-make-definition-context intdef-ctx))
                 (define (intro stx)
                   (internal-definition-context-introduce intdef-ctx* stx))

                 (syntax-local-bind-syntaxes (list #'x-) #f intdef-ctx*)
                 (syntax-local-bind-module #'x #'x- #'A.expansion intdef-ctx*)]

           #:with x-- (intro #'x-)
           #:with {~var B* (sig intdef-ctx*)} #'B
           #:with B** (reintroduce-#%dot (intro #'x) #'x-- #'B*.expansion intdef-ctx*)

           #:attr expansion (~>> (syntax/loc/props this-syntax
                                                   (head ([x-- A.expansion])
                                                         B**))
                                 (internal-definition-context-track intdef-ctx*))
           #:attr residual (residual #'[A.residual B*.residual expansion]
                                     #'head)])

;; expects result from partial-decl.expansion as input, outputs
;; a fully expanded decl in .expansion.
(define-syntax-class (expanded-decl intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  ;; (#%val-decl type)
  [pattern (val-decl:#%val-decl (~var val-type (type intdef-ctx)))
           #:attr expansion (syntax/loc/props this-syntax
                                              (val-decl val-type.expansion))
           #:attr residual (residual #'[val-type.residual expansion]
                                     #'val-decl)]

  ;; (#%constructor-decl type)
  [pattern (constr-decl:#%constructor-decl (~var constr-type (type intdef-ctx)))
           #:attr expansion (syntax/loc/props this-syntax
                              (constr-decl constr-type.expansion))
           #:attr residual (residual #'[constr-type.residual expansion]
                                     #'constr-decl)]

  ;; (#%type-decl (#%alias type))
  [pattern (type-decl:#%type-decl
            (alias:#%alias (~var alias-type (type intdef-ctx))))
           #:attr expansion (syntax/loc/props this-syntax
                                              (type-decl (alias alias-type.expansion)))
           #:attr residual (residual #'[alias-type.residual expansion]
                                     #'type-decl)]

  ;; (#%type-decl (#%opaque))
  [pattern (type-decl:#%type-decl (opaque:#%opaque))
           #:attr expansion this-syntax
           #:attr residual (residual #'[expansion]
                                     #'type-decl)]

  ;; (#%type-decl (#%data constructor-id ...))
  [pattern (type-decl:#%type-decl
            (data:#%data {~var constructor-id (ref-id intdef-ctx)} ...))
           #:attr expansion (syntax/loc/props this-syntax
                              (type-decl (data constructor-id.expansion ...)))
           #:attr residual (residual #'[constructor-id.residual ... expansion]
                                     #'type-decl)]

  ;; (#%module-decl signature)
  [pattern (module-decl:#%module-decl
            {~var signature (sig intdef-ctx)})
           #:attr expansion (syntax/loc/props this-syntax
                              (module-decl signature.expansion))
           #:attr residual (residual #'[signature.residual expansion]
                                     #'module-decl)]

  )

;; ---------------------------------------------

;; expects once local-expanded syntax as input, outputs
;; partially expanded signature.
(define-syntax-class (partial-expanded-sig intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  [pattern (sig:#%sig
            internal-ids:hash-literal
            decls:hash-literal)

           #:with [{~var decl- (partial-decl intdef-ctx)} ...]
           (attribute decls.values)
           #:with decls-
           (hash-zip (attribute decls.keys) (attribute decl-.expansion))
           #:with expansion
           (syntax/loc/props this-syntax
             (sig internal-ids decls-))
           #:attr residual (residual #'[decl-.residual ... expansion] #'sig)]

  [pattern (pi:#%pi-sig . rst)
           #:with expansion this-syntax
           #:with residual (residual #'[expansion] #'pi)])

;; expects once local-expanded decl as input, outputs
;; partially expanded decl.
(define-syntax-class (partial-expanded-decl intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  #:commit
  #:literal-sets [sig-literals]

  [pattern (module-decl:#%module-decl
            {~var s (partial-sig intdef-ctx)})
           #:with expansion #'(module-decl s.expansion)
           #:with residual (residual #'[s.residual expansion] #'module-decl)]

  [pattern (head . rst)
           #:with expansion this-syntax
           #:with residual (residual #'[expansion] #'head)])

;; ---------------------------------------------

;; sig-where : Sig Symbol Type -> Sig
(define (sig-where base sym type)
  (syntax-parse base
    #:literal-sets [sig-literals]
    [(sig:#%sig internal-ids:hash-literal decls:hash-literal)
     #`(sig
        internal-ids
        #,(hash-update (attribute decls.value)
                       (namespaced:type sym)
            (λ (prev-decl)
              (syntax-parse prev-decl #:literal-sets [sig-literals]
                [(type-decl:#%type-decl (#%opaque))
                 #`(type-decl (#%alias #,type))]
                [_
                 (raise-syntax-error #f
                   "can't `where` a non-opaque declaration"
                   base)]))
            (λ ()
              (raise-syntax-error #f
                "can't `where` a non-existent declaration"
                base))))]))
