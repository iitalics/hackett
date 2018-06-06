#lang racket/base
(require
 "../namespace/reqprov.rkt"
 (only-in (unmangle-in "../sig.rkt") sig type val)
 (only-in (unmangle-in hackett/base) Integer ->)
 ;; -----------
 (for-syntax
  racket/base
  racket/syntax
  racket/pretty
  syntax/parse
  "../check/module-var.rkt"    ; bind-module
  "../rep/sig.rkt"             ; expand-sig
  "../rep/sig-pretty.rkt"      ; sig->string
  "../namespace/namespace.rkt" ; *-namespace-introduce
  ))

(module+ test
  (begin-for-syntax
    (require rackunit)

    (define S
      (expand-sig
       (signature-namespace-introduce
        #'(sig (type T = Integer)
               (type U = T)))))

    (displayln "; --------------------------------------------")
    (displayln (sig->string S))
    (newline)

    ;; ---------------------------------------

    (define/syntax-parse m-
      (generate-temporary 'm))

    (define ic
      (syntax-local-make-definition-context))

    (syntax-local-bind-syntaxes (list #'m) #f ic)
    (syntax-local-bind-module #'m #'m- S ic)

    ;; TODO: try expanding a #%dot_τ

    ))
