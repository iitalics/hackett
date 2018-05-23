#lang racket/base

(provide for/free-id-table
         hash-literal
         (rename-out [hash-literal hash-lit]))

(require syntax/parse/define
         syntax/parse
         syntax/id-table)

(define-syntax-class hash-literal
  [pattern stx
    #:attr value (syntax-e #'stx)
    #:when (hash? (attribute value))
    #:attr keys (hash-keys (attribute value))
    #:attr values (hash-values (attribute value))])

(define-simple-macro (for/free-id-table (clause ...) body ...)
  (for/fold ([tbl (make-immutable-free-id-table)])
            (clause ...)
    (let-values ([(k v) (let () body ...)])
      (free-id-table-set tbl k v))))
