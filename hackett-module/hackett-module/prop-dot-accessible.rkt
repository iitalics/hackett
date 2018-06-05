#lang racket/base

(provide prop:dot-accessible
         dot-accessible
         dot-accessible?)

(require struct-like-struct-type-property)

;; key->id : Key -> [Maybe Id]
;; such that the id will expand to someting that has the
;; correct type already
(define-struct-like-struct-type-property dot-accessible
  [key->id])

