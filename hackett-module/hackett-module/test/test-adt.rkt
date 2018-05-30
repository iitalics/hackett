#lang hackett-module/outside
(require hackett/private/test)

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

(def-module FD
  (appₘ F D))

(def-module F*
  ; eta-expand F
  (λₘ ([Q : (sig (data B T F))])
    (appₘ F Q)))

(def-module F*D
  (appₘ F* D))

(test {(FD.f D.T) ==! 1})
(test {(FD.f D.F) ==! 0})

; note: typechecks the same
(test {(F*D.f D.T) ==! 1})
(test {(F*D.f D.F) ==! 0})

#|
(λ (M-)
  (define-syntax M
    (.... M- .... B T F constructor:M.T33 constructor:M.F34 ....))
  (define-syntax constructor:M.T33 (....))
  (define-syntax constructor:M.F34 (....))

  ;; we set up `#%dot_e` and `M` so that `(#%dot_e M T)`
  ;; expands in a pattern context to `constructor:M.T33`, which
  ;; is a valid hackett pattern expression.
  (define f
    (λ (x)
      (case x
        [(#%dot M T) 1]
        [(#%dot M F) 0])))
  (l:mod
   (hash 'f f)
   (hash)))
|#
