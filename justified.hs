member :: Ord k => k -> MapK s k v -> Maybe (k {$\in$} s)

newtype JMap s k v = JMap (Map k v)
    deriving Functor

newtype JKey s k = Element k

instance The (JMap s k v) (Map k v)
instance The (JKey s k)  k

member ::  k -> JMap s k v -> Maybe (JKey s k)

lookup   :: JKey s k -> JMap s k v -> v

reinsert
  :: JKey s k -> v -> JMap s k v -> JMap s k v

withMap
:: Map k v  -> (forall s. JMap s k v -> t) -> t
