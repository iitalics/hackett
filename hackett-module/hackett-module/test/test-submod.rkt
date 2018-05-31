#lang hackett-module

(def-signature S
  (sig
    (module Sub :
      (sig
        (type T)))

    (type R = Sub.T)
    ))
