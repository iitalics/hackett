#lang hackett-module/inside

(type X Integer)

(defn foo : (∀ [a] {a -> (Tuple a X)})
  [[x] (Tuple (: x a) 5)])

{1 + 1}
