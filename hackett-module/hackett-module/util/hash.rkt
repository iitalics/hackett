#lang racket/base

(provide hash-zip)

;; hash-zip : [Listof K] [Listof V] -> [Hashof K V]
(define (hash-zip ks vs)
  (for/hash ([k (in-list ks)] [v (in-list vs)])
    (values k v)))
