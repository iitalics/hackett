#lang racket/base
(require
 (only-in hackett/private/base :)
 (only-in hackett/private/adt defn data type-constructor-spec)
 (for-syntax racket/base)
 syntax/parse/define)

(provide
 def-record)

(begin-for-syntax
  (define-splicing-syntax-class record-field-spec
    #:literals [:]
    [pattern {~seq id:id : type}]))

(define-simple-macro
  (def-record τ:type-constructor-spec
    field:record-field-spec ...)
  #:with [spec ...] #'τ
  #:with pat #'(τ.tag field.id ...)
  (begin
    (data spec ... (τ.tag field.type ...))
    (defn field.id [[pat] field.id]) ...))
