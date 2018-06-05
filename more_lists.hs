data Zero    -- no constructors, ghosts need no body
data NonZero
data x + y

nil :: List Zero a
nil = List []

gdpHead :: List NonZero a -> a
gdpHead xs = head (the xs) -- safe!

gdpRev :: List n a -> List n a
gdpRev = coerce . reverse . coerce

classify :: [a] -> Either (List Zero a) (List NonZero a)
classify xs = case xs of
  []    -> Left  (coerce xs)
  (_:_) -> Right (coerce xs)

(+++) :: List n a -> List m a -> List (n + m) a
xs +++ ys = coerce (the xs ++ the ys)

zero_neutral :: List (Zero + n) a -> List n a
zero_neutral = coerce

add_commutes :: List (n + m) a -> List (m + n) a
add_commutes = coerce
