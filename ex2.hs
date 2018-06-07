dot :: ([Double] ~~ vec1 ::: Length vec1 == n)
    -> ([Double] ~~ vec2 ::: Length vec2 == n)
    -> Double
dot vec1 vec2 = sum (zipWith (*) vec1 vec2)

-- Compute the dot product of a list with its reverse.
dot_rev :: [Double] -> Double
dot_rev xs = name xs $ \vec ->
  dot (vec ...refl) (reverse vec ...rev_length)
