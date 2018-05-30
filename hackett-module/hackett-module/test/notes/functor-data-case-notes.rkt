#lang hackett-module/outside

(def-module F
  (λₘ ([M : (sig (data B T F))])
    (mod
     (def f
       (λ (x)
         (case x
           [M.T 1]
           [M.F 0]))))))

(def-module D
  (mod
   (data B T F)))

#;
(def-module FD
  (appₘ F D))

#|
==>
(λ (M-)
  (define-syntax M
    (.... M- .... B T F constructor:M.T33 constructor:M.F34 ....))
  (define-syntax constructor:M.T33 (....))
  (define-syntax constructor:M.F34 (....))

  ;; we need to set up `#%dot` and `M` so that something like
  ;; `(#%dot M T)` can expand *in a pattern context* to
  ;;  `constructor:M.T33`.
  (define f
    (λ (x)
      (case x
        [(#%dot M T) 1]
        [(#%dot M F) 0])))
  (hash 'f f))

|#
