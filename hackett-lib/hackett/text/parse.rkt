#lang hackett
(require
 hackett/data/list)

(data (Parse s a) (Parse {(List s) -> (List (Tuple a (List s)))}))

(defn parse : (forall [s a] {(Parse s a) -> (List s) -> (List (Tuple a (List s)))})
  [[(Parse p)] p])

;; --------
;; utils

(defn first : (forall [a b c] {{a -> b} -> (Tuple a c) -> (Tuple b c)})
  [[f (Tuple x y)] (Tuple (f x) y)])

(defn second : (forall [a b c] {{a -> b} -> (Tuple c a) -> (Tuple c b)})
  [[f (Tuple x y)] (Tuple x (f y))])

(defn uncurry : (forall [a b c] {{a -> b -> c} -> (Tuple a b) -> c})
  [[f (Tuple x y)] (f x y)])

;; --------
;; instances

(instance (forall [s] (Functor (Parse s)))
  [map
   (λ [f p] (Parse (λ [in] (map (first f) (parse p in)))))])

(instance (forall [s] (Applicative (Parse s)))
  [pure
   (λ [x] (Parse (λ [in] {(Tuple x in) :: Nil})))]
  [<*>
   (λ [pf px]
     (Parse
      (λ [in]
        {(parse pf in)
         >>=
         (λ* [[(Tuple f out)]
              {(parse px out) <&> (first f)}])})))])

(instance (forall [s] (Monad (Parse s)))
  [=<<
   (λ [f px]
     (Parse
      (λ [in]
        {(parse px in)
         <&>
         (first f)
         >>=
         (uncurry parse)})))])

;; --------
;; combinators
