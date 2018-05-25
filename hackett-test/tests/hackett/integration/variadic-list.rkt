#lang hackett
(require hackett/private/test)

(def my-empty : (forall [a] (List a)) (list))

(defn last : (forall [a] {(List a) -> a})
  [[(list)]    (error! "last of empty list")]
  [[(list x)]  x]
  [[{x :: xs}] (last xs)])

(test {(list 1 2 3) ==! {1 :: 2 :: 3 :: Nil}})
(test {(head! (last (list (list 8 9) (list 10 100)))) ==! 10})
