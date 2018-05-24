#lang racket/base

(provide configure)

(define (configure data)
  (read-cdot #t))

