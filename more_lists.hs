data Zero    -- no constructors, ghosts need no body
data NonZero
data x + y

nil :: ([a] ~~ Nil)
nil = defn []

gdpHead :: ([a] ~~ xs ::: IsCons xs) -> (a ~~ Head xs)
gdpHead xs = defn (head (the xs)) -- safe!

gdpRev :: ([a] ~~ xs) -> ([a] ~~ Reverse xs)
gdpRev = defn . reverse . coerce

classify :: ([a] ~~ xs) -> Either (Proof (IsCons xs)) (Proof (IsNil xs))
classify xs = case xs of
  []    -> Left sorry
  (_:_) -> Right sorry

(+++) :: ([a] ~~ xs)
      -> ([a] ~~ ys)
      -> ([a] ~~ xs +++ ys)
xs +++ ys = defn (the xs ++ the ys)

-- Lemmas
list_shape :: Proof (IsNil xs || IsCons xs)
list_shape = axiom

rev_cons :: IsCons xs -> Proof (IsCons (Reverse xs))
rev_cons _ = axiom

length_spec :: Proof ( (IsNil xs && Length xs == Zero)
  || (IsCons xs && Length xs == Succ (Length (Tail xs))) )

length_sum :: Proof (Length (xs +++ ys) == Length xs + Length ys)
length_Sum = sorry

rev_length :: Proof (Length (Reverse xs) == Length xs)
rev_length = sorry

zero_neutral :: Proof (n == Zero + n)
zero_neutral = sorry

add_commutes :: Proof (n + m == m + n)
add_commutes = sorry
