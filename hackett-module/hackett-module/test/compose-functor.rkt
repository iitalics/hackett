#lang hackett-module/outside

(def-signature TRANS
  (sig (type In)
       (type Out)
       (val f : (-> In Out))))

(def-module Thrush
  (λₘ ([A : TRANS])
    (λₘ ([B : (where TRANS In = A.Out)])
      (mod
       (type In A.In)
       (type Out B.Out)
       (def f : {In -> Out}
            (λ (x) (B.f (A.f x))))))))

(def-module Even
  (mod (type In Integer)
       (type Out Bool)
       (def f : {In -> Out} (λ* [[0] True] [[_] False]))))

(def-module Lambool
  (mod (type In Bool)
       (type Out {Integer -> Integer -> Integer})
       (def f : {In -> Out} (λ (b) (λ (t f) (if b t f))))))

(def-module EvenLambool (appₘ (appₘ Thrush Even) Lambool))

