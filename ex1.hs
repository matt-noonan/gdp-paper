-- API functions
gdpRev :: ([a] ~~ xs) -> ([a] ~~ Reverse xs)
gdpRev xs = defn (reverse (the xs))

length :: ([a] ~~ xs) -> (Integer ~~ Length xs)
length xs = defn (Prelude.length (the xs))

zipWith :: (a -> b -> c)
         -> ([a] ~~ xs ::: Length xs == n)
         -> ([a] ~~ ys ::: Length ys == n)
         -> [a]
zipWith f xs ys = Prelude.zipWith f (the xs) (the ys)

-- Names for API functions
newtype Length  xs = Length  Defn
newtype Reverse xs = Reverse Defn

-- Lemmas (all bodies are `axiom`)
rev_length :: Proof (Length (Reverse xs) == Length xs)
rev_rev    :: Proof (Reverse (Reverse xs) == xs)
rev_cons   :: IsCons xs -> Proof (IsCons (Reverse xs))

data ListCase xs = IsNil  (Proof (IsNil  xs))
                 | IsCons (Proof (IsCons xs))

classify :: ([a] ~~ xs) -> ListCase xs
classify xs = case the xs of
  []    -> IsNil  axiom
  (_:_) -> IsCons axiom
