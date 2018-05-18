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

(instance (forall [s a] (Semigroup a) => (Semigroup (Parse s a)))
  [++ (λ [px py] {++ <$> px <*> py})])

(instance (forall [s a] (Monoid a) => (Monoid (Parse s a)))
  [mempty (pure mempty)])

;; --------
;; combinators

(def any : (forall [s] (Parse s s))
  (Parse
   (λ* [[Nil] Nil]
       [[{x :: xs}] {(Tuple x xs) :: Nil}])))

(defn when : (forall [s a] {{a -> Bool} -> (Parse s a) -> (Parse s a)})
  [[f px]
   (Parse
    (λ [in]
      (filter {f . fst} (parse px in))))])

(defn one-of : (forall [s] (Eq s) => {(List s) -> (Parse s s)})
  [[xs] (when (flip elem? xs) any)])

(defn +++ : (forall [s a] {(Parse s a) -> (Parse s a) -> (Parse s a)})
  [[px py] (Parse (λ [in] {(parse px in) ++ (parse py in)}))])

(defn seq : (forall [s a b] {(Parse s a) -> (Parse s b) -> (Parse s (Tuple a b))})
  [[px py] {Tuple <$> px <*> py}])

(defn many : (forall [s a] {(Parse s a) -> (Parse s (List a))})
  [[p] {(pure Nil) +++ (many1 p)}])

(defn many1 : (forall [s a] {(Parse s a) -> (Parse s (List a))})
  [[p] {:: <$> p <*> (many p)}])

(defn sep-by : (forall [s a sep] {(Parse s a) -> (Parse s sep) -> (Parse s (List a))})
  [[p sep]
   {:: <$> p <*> (many {(flip const) <$> sep <*> p})}])
