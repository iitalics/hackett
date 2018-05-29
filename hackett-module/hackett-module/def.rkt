#lang racket/base
(require
 "mod.rkt"
 "rep/sig.rkt"
 racket/pretty
 syntax/parse/define
 hackett/private/type-language
 (prefix-in hkt: hackett/base)
 (prefix-in sig: "sig.rkt")
 (for-syntax racket/base
             syntax/parse))

(provide
 def-module)

(define-syntax-parser def-module
  [(_ name:id m:expr)
   #:with m- (local-expand #'m 'module-begin '())
   #'(pretty-write 'm-)])
