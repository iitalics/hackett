#lang racket/base

(require syntax/parse/define
         syntax/macro-testing
         (only-in hackett/private/base Integer)
         "../sig.rkt"
         "../dot.rkt"
         "../rep/sig-literals.rkt"
         (for-syntax racket/base
                     rackunit
                     rackunit/text-ui
                     syntax/parse
                     syntax/parse/define
                     "../check/sig-matches.rkt"
                     "../rep/sig.rkt"
                     ))

(def-signature CheckStxArrows
  (sig
   (type X)
   (val x :   X)
   (val y : X)))

(begin-for-syntax
  (define-binary-check (check-sig-matches A B)
    (signature-matches? A B))
  (define-binary-check (check-not-sig-matches A B)
    (not (signature-matches? A B)))

  (define-simple-macro (sig . stuff) (expand-sig #`(sig . stuff)))
  (define-simple-macro (pi . stuff) (expand-sig #`(Π . stuff)))

  )

(define-for-syntax tests
  (test-suite "tests"
    (test-case "S"
      (define S
        (expand-sig
         #'(sig
            (type X)
            (val x : X))))

      (check sig-matches?
             S
             (expand-sig
              #'(sig
                 (type X)
                 (val x : X))))

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type X = Integer)
                 (val x : X)))
             S)

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type Y)
                 (type X = Y)
                 (val x : X)))
             S)

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type X = Integer)
                 (val x : Integer)))
             S)

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type Y)
                 (type X = Y)
                 (val x : Y)))
             S)

      ;; -----------------

      (check-sig-matches
       (sig (val v : Integer)
            (type X = Integer)
            (type Y = Integer))
       (sig (val v : X)
            (type X)
            (type Y = X)))

      (check-sig-matches
       (sig (type X)
            (type Y = X))
       (sig (type Y)))

      ;; ---------------------

      (with-syntax ([I  (sig (type t))]
                    [I* (sig (type t = Integer))]
                    [J  (sig (type s) (type t))]
                    [J* (sig (type s) (type t = s))])
        (check-sig-matches #'I* #'I)
        (check-sig-matches #'J* #'J)

        #|
actual:
        #<syntax:/home/milo/Git/hackett/hackett-module/hackett-module/sig.rkt:46:5
        (#%pi-sig ((x22 (#%sig #hash((t . t))
                               #hash((t . (#%type-decl (#%opaque)))))))
                  (#%sig #hash((v . v))
                         #hash((v . (#%val-decl (#%type:con opaque:x.t23))))))>
expected:
        #<syntax:/home/milo/Git/hackett/hackett-module/hackett-module/sig.rkt:46:5
        (#%pi-sig ((x24 (#%sig #hash((t . t))
                               #hash((t . (#%type-decl (#%alias (#%type:con Integer))))))))
                  (#%sig #hash((v . v))
                         #hash((v . (#%val-decl (#%type:con Integer))))))>
|#

        (check-sig-matches
         (pi ([x : I]) (sig (val v : (#%dot_τ x t))))
         (pi ([x : I*]) (sig (val v : (#%dot_τ x t)))))

        (check-sig-matches
         (pi ([x : I]) (sig (val v : (#%dot_τ x t))))
         (pi ([x : I*]) (sig (val v : Integer))))

        (check-sig-matches
         (pi ([x : J]) (sig (val v : (#%dot_τ x t))))
         (pi ([x : J*]) (sig (val v : (#%dot_τ x t)))))

        (check-sig-matches
         (pi ([x : J]) (sig (val v : (#%dot_τ x t))))
         (pi ([x : J*]) (sig (val v : (#%dot_τ x s)))))

        (check-not-sig-matches
         (pi ([x : J]) (sig (val v : (#%dot_τ x t))))
         (pi ([x : J]) (sig (val v : Integer)))))

    )))

(module+ test
  (convert-compile-time-error
   (let-syntax
       ([m
         (λ (stx)
           #`'#,(run-tests tests))])
     (m))))
