#lang hackett-module/outside

(require hackett/private/test)

(def-signature S
  (sig
   (type T = Integer)
   (val f : {T -> Integer})))

(def-signature S*
  (sig
   (type T)
   (val f : {T -> Integer})))

(def-module M
  (mod
   (type T Integer)
   (def f : {T -> Integer} (λ (x) 5))))

(def-module M* (seal M :> S*))

(def-signature BOOL-REP
  (sig
   (type Boool)
   (val true : Boool)
   (val false : Boool)
   (val if : (∀ (X) {Boool -> X -> X -> X}))))

(def-signature NAT-REP
  (sig
   (type Boool)
   (type Nat)
   (val z : Nat)
   (val z? : {Nat -> Boool})
   (val add1 : {Nat -> Nat})
   ;; sub1 raises an error if its argument is z
   (val sub1 : {Nat -> Nat})))

(def-signature NAT
  (Π ([B : BOOL-REP])
    (Π ([N : (where NAT-REP Boool = B.Boool)])
      (sig
       ;; equality
       (val = : {N.Nat -> N.Nat -> B.Boool})
     
       ;; arithmetic
       (val + : {N.Nat -> N.Nat -> N.Nat})))))

(def-module Bool-Rep
  (seal
   (mod
    (type Boool (∀ (X) {X -> X -> X}))
    (def true : Boool (λ (t f) t))
    (def false : Boool (λ (t f) f))
    (def if : (∀ (X) {Boool -> X -> X -> X})
         (λ (b t f) (b t f))))
   :>
   BOOL-REP))

(def-module Nat
  (seal
   (λₘ ([B : BOOL-REP])
     (λₘ ([N : (where NAT-REP Boool = B.Boool)])
       (mod
        ;; comparison
        (def = : {N.Nat -> N.Nat -> B.Boool}
             (λ (a b)
               (B.if
                (N.z? a)
                (N.z? b)
                (B.if (N.z? b)
                      B.false
                      (= (N.sub1 a) (N.sub1 b))))))
        ;; arithmetic
        (def + : {N.Nat -> N.Nat -> N.Nat}
             (λ (a b)
               (B.if (N.z? a)
                     b
                     (N.add1 (+ (N.sub1 a) b))))))))
   :>
   NAT))

(def-module Nat-Rep/Int
  (seal
   (mod (type Boool Bool-Rep.Boool)
        (type Nat Integer)
        (def z : Nat 0)
        (def z? : (-> Nat Boool) (λ (x) (if (== x 0) Bool-Rep.true Bool-Rep.false)))
        (def add1 : (-> Nat Nat) (λ (x) (+ x 1)))
        ;; sub1 raises an error if its argument is z
        (def sub1 : (-> Nat Nat) (λ (x) (- x 1))))
   :>
   (where NAT-REP Boool = Bool-Rep.Boool)))
    
(def-module Nat/Int
  (appₘ (appₘ Nat Bool-Rep) Nat-Rep/Int))

(def-module TestNat
  (λₘ ([NR : (where NAT-REP Boool = Bool-Rep.Boool)])
    (mod
     (def-module N (appₘ (appₘ Nat Bool-Rep) NR))
     (type Boool Bool-Rep.Boool)
     (type Nat NR.Nat)
     (def true : Boool Bool-Rep.true)
     (def false : Boool Bool-Rep.false)
     (def n0 : Nat NR.z)
     (def n1 : Nat (NR.add1 n0))
     (def n2 : Nat (NR.add1 n1))
     (def n3 : Nat (NR.add1 n2))
     (def n4 : Nat (NR.add1 n3))
     (def n5 : Nat (NR.add1 n4))
     (def n6 : Nat (NR.add1 n5))
     (def n7 : Nat (NR.add1 n6))
     (def n8 : Nat (NR.add1 n7))
     (def n9 : Nat (NR.add1 n8))

     (def should-be-true : Bool
       (Bool-Rep.if (N.= (N.+ n3 n5) n8)
                         True
                         False)))))

(def-module TestNat/Int
  (appₘ TestNat Nat-Rep/Int))

(test {TestNat/Int.should-be-true ==! True})
