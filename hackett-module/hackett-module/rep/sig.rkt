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

(define-simple-macro
  (define-expansion-class name:id desc:str expanded-name:id literal-ids:expr)
  (define-syntax-class (name [intdef-ctx #f])
    #:description desc
    #:attributes [expansion residual]
    [pattern stx
             #:with {~var || (expanded-name intdef-ctx)}
             (local-expand #'stx 'expression literal-ids intdef-ctx)]))

(define-expansion-class ref-id "id reference" expanded-id sig-literal-ids)
(define-expansion-class sig "sig" expanded-sig sig-literal-ids)
(define-expansion-class decl "sig declaration" expanded-decl sig-literal-ids)

(define (expand-sig stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var S (sig intdef-ctx)}
     #'S.expansion]))

(define (expand-decl stx [intdef-ctx #f])
  (syntax-parse stx
    [{~var D (decl intdef-ctx)}
     #'D.expansion]))

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

(define-syntax-class (expanded-id intdef-ctx)
  #:description #f
  #:attributes [expansion residual]
  [pattern expansion:id
           #:with residual (residual #'[] #'expansion)])

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
                 (syntax-local-bind-syntaxes
                  (attribute internal-ids.values)
                  #f
                  intdef-ctx*)]
           #:with internal-ids- (intro #'internal-ids)
           #:with [{~var decl- (decl intdef-ctx*)} ...] (attribute decls.values)
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

  )

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
