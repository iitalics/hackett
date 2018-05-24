#lang racket/base

(provide (rename-out [dot-exp-read read]
                     [dot-exp-read-syntax read-syntax]
                     [dot-exp-get-info get-info]))

(require (only-in syntax/module-reader
                  make-meta-reader
                  lang-reader-module-paths))

(define (wrap-reader orig-rd)
  (define (rd . args)
    (parameterize ([read-cdot #true])
      (define result (apply orig-rd args))
      (cond
        [(syntax? result)
         (define old-prop (syntax-property result 'module-language))
         (define new-prop `#[dot-exp/lang/language-info get-language-info ,old-prop])
         (syntax-property result 'module-lanugage new-prop)]
        [else
         result])))
  rd)

(define-values (dot-exp-read dot-exp-read-syntax dot-exp-get-info)
  (make-meta-reader
   'dot-exp
   "language path"
   lang-reader-module-paths
   wrap-reader
   wrap-reader
   (lambda (proc) proc)))

