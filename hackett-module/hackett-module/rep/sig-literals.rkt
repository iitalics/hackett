#lang racket/base

(provide #%sig
         #%pi-sig
         #%val-decl
         #%constructor-decl
         #%type-decl
         #%alias
         #%opaque
         (for-syntax sig-literals
                     sig-literal-ids
                     ; ---
                     sig-internal-ids
                     sig-decls
                     decl-type?
                     decl-type-opaque?
                     decl-val?))

(require (for-syntax racket/base
                     syntax/parse
                     "../util/stx.rkt"))

;; (#%sig
;;   #:hash([name . internal-id]
;;          ...)
;;   #:hash([name . decl]
;;          ...))
;; where the name sets are the same,
;; and all the internal-ids are bound
;; in all the decls
(define-syntax #%sig #f)

;; (#%pi-sig
;;   ([param-id sig])
;;   sig)
(define-syntax #%pi-sig #f)

;; A Decl is one of:
;;  - (#%val-decl Type)
;;  - (#%constructor-decl Type)
;;  - (#%type-decl (#%alias Type))
;;  - (#%type-decl (#%opaque))

(define-syntax #%val-decl #f)
(define-syntax #%constructor-decl #f)
(define-syntax #%type-decl #f)
(define-syntax #%alias #f)
(define-syntax #%opaque #f)

(begin-for-syntax
  (define sig-literal-ids
    (list #'#%sig
          #'#%pi-sig
          #'#%val-decl
          #'#%constructor-decl
          #'#%type-decl #'#%alias #'#%opaque))

  (define-literal-set sig-literals
    [#%sig
     #%pi-sig
     #%val-decl
     #%constructor-decl
     #%type-decl #%alias #%opaque]))

;; -----------------------------------------------------------------

(begin-for-syntax

  ;; Sig -> [Hashof Symbol Identifier]
  (define (sig-internal-ids s)
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(sig:#%sig internal-ids:hash-literal _)
       (attribute internal-ids.value)]))

  ;; Sig -> [Hashof Symbol Decl]
  (define (sig-decls s)
    (syntax-parse s
      #:literal-sets [sig-literals]
      [(sig:#%sig _ decls:hash-literal)
       (attribute decls.value)]))

  ;; Decl -> Bool
  (define (decl-type? d)
    (syntax-parse d
      [(#%type-decl _) #t]
      [_ #f]))

  ;; Decl -> Bool
  (define (decl-type-opaque? d)
    (syntax-parse d
      #:literal-sets [sig-literals]
      [(#%type-decl (#%opaque)) #t]
      [_ #f]))

  ;; Decl -> Bool
  (define (decl-val? d)
    (syntax-parse d
      #:literal-sets [sig-literals]
      [(#%val-decl _) #t]
      [_ #f]))

  ;; Decl -> Bool
  (define (decl-constructor? d)
    (syntax-parse d
      #:literal-sets [sig-literals]
      [(#%constructor-decl _) #t]
      [_ #f]))

  )
