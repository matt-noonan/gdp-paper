class The d a | d -> a where
    the :: d -> a
    default the :: Coercible d a => d -> a
    the = coerce

instance The (List n a) a
