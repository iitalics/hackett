#lang racket/base

(provide #%sig
         #%val-decl
         #%type-decl
         #%alias
         #%opaque
         (for-syntax sig
                     decl
                     expand-sig
                     expand-decl
                     sig-literals
                     ))

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse/define
                     syntax/intdef
                     threading
                     hackett/private/util/stx
                     hackett/private/typecheck
                     "../util/stx.rkt"
                     (for-syntax racket/base)))

;; (#%sig
;;   #:hash([name . internal-id]
;;          ...)
;;   #:hash([name . decl]
;;          ...))
;; where the name sets are the same,
;; and all the internal-ids are bound
;; in all the decls
(define-syntax #%sig #f)

;; A Decl is one of:
;;  - (#%val-decl Type)
;;  - (#%type-decl (#%alias Type))
;;  - (#%type-decl (#%opaque))

(define-syntax #%val-decl #f)
(define-syntax #%type-decl #f)
(define-syntax #%alias #f)
(define-syntax #%opaque #f)

(begin-for-syntax
  (define sig-literal-ids
    (list #'#%sig
          #'#%val-decl #'#%type-decl #'#%alias #'#%opaque))

  (define-literal-set sig-literals
    [#%sig
     #%val-decl #%type-decl #%alias #%opaque])

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
  
  ;; ---------------------------------------------


  (define-syntax-class (expanded-sig intdef-ctx)
    #:description #f
    #:attributes [expansion residual]
    #:commit
    #:literal-sets [sig-literals]

    ;; (#%sig
    ;;   #:hash([name . internal-id]
    ;;          ...)
    ;;   #:hash([name . decl]
    ;;          ...))
    [pattern (head:#%sig
              internal-ids:hash-literal
              decls:hash-literal)
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
      (for/hash ([name (in-list (attribute decls.keys))]
                 [decl- (in-list (attribute decl-.expansion))])
        (values name decl-))
      #:attr expansion (~>> (syntax/loc/props this-syntax
                              (head internal-ids-
                                    decls-expansion-))
                            (internal-definition-context-track intdef-ctx*))
      #:attr residual (residual #'[decl-.residual ... expansion]
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
                                #'type-decl)])

  )
      


