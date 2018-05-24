#lang dot-exp racket/base

(require syntax/parse/define
         syntax/macro-testing
         hackett/private/type-reqprov
         (only-in (unmangle-types-in hackett/private/kernel) ∀ -> #%app)
         (only-in hackett/private/base Integer)
         (only-in (unmangle-types-in hackett-module/outside) #%dot)
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

  (define-simple-macro (sig stuff |...|) (expand-sig #`(sig stuff |...|)))
  (define-simple-macro (pi stuff |...|) (expand-sig #`(Π stuff |...|)))

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
           S))

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

  (check-sig-matches
   (pi ((B110 :
         (sig
          (val false : Boool)
          (val true : Boool)
          (val if : (∀ (X) {Boool -> X -> X -> X}))
          (type Boool))))
      (Π ((N112 :
           (sig
            (val z? : {Nat -> Boool})
            (val add1 : {Nat -> Nat})
            (val sub1 : {Nat -> Nat})
            (type Nat)
            (val z : Nat)
            (type Boool = B110.Boool))))
         (sig
          (val = : {N112.Nat -> N112.Nat -> B110.Boool})
          (val + : {N112.Nat -> N112.Nat -> N112.Nat}))))
   (pi ((B17106 :
         (sig
          (val false : Boool)
          (val true : Boool)
          (val if : (∀ (X) {Boool -> X -> X -> X}))
          (type Boool))))
      (Π ((N19108 :
           (sig
            (val z? : {Nat -> Boool})
            (val add1 : {Nat -> Nat})
            (val sub1 : {Nat -> Nat})
            (type Nat)
            (val z : Nat)
            (type Boool = B17106.Boool))))
         (sig
          (val = : {N19108.Nat -> N19108.Nat -> B17106.Boool})
          (val + : {N19108.Nat -> N19108.Nat -> N19108.Nat})))))
  

  )
