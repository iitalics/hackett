#lang racket/base

(provide traverse-stx/recur)

(require racket/list
         racket/struct)

;; traverse-stx/recur : Stx [Stx -> Stx] -> Stx
;; Note that rec must be able to take both syntax and
;; non-stx-but-containing-stx values, in other words,
;; rec must be able to take something that might come
;; from syntax-e
(define (traverse-stx/recur stx rec)
  (define e (stx-e stx))
  (restore
   stx
   (cond
     [(atomic-literal? e) stx]
     [(cons? e)
      (cons (rec (car e))
            (rec (cdr e)))]
     [(box? e)
      (box-immutable (rec (unbox e)))]
     [(vector? e)
      (apply vector-immutable (map rec (vector->list e)))]
     [(hash? e)
      (for/hash ([(k v) (in-hash e)])
        (values k (rec v)))]
     [(prefab-struct-key e)
      (define key (prefab-struct-key e))
      (apply make-prefab-struct key (map rec (struct->list e)))]
     [else
      (error 'traverse-stx "unrecognized syntax: ~v" stx)])))

;; stx-e : Stx -> E
(define (stx-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; restore : Stx E -> Stx
(define (restore stx e)
  (if (syntax? stx)
      (let ([stx* (syntax-disarm stx #f)])
        (syntax-rearm (datum->syntax stx* e stx* stx*) stx))
      e))

;; atomic-literal? : E -> Boolean
(define (atomic-literal? e)
  (or (null? e) (boolean? e) (number? e) (symbol? e)
      (string? e) (bytes? e)
      (regexp? e) (byte-regexp? e)))

