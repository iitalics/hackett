#lang hackett-module/outside

(def-signature EMPTY
  (sig))

(def-signature NUMBERS
  (sig
    (val x : Integer)
    (val y : Integer)
    (val z : Integer)))

(data Order LT GT EQ)

(def-signature ORD
  (sig
    (type T)
    (val compare : {T -> T -> Order})))

#;
(def-signature MAKE_MAP
  (Π ([Elem : ORD])
    (sig
      (type T)
      (val empty : T)
      (val add : {Elem.T -> T -> T})
      (val rem : {Elem.T -> T -> T})
      (val elem? : {Elem.T -> T -> Bool}))))

; =====================================
#|

(def-module Foo
  (seal (mod (def x 3))
        :> EMPTY))

(def-module N
  (seal (mod (def x 4)
             (def y x)
             (def z {x + 1}))
        :> NUMBERS))

(def-module N-empty
  (seal N :> EMPTY))

|#
