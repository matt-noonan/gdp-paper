newtype JMap ks k v = JMap (Map k v)
    deriving Functor

newtype k #$\in$# ks = Element k

instance The (JMap ks k v) (Map k v)
instance The (k #$\in$# ks)  k

member ::  k -> JMap ks k v -> Maybe (k #$\in$# ks)

lookup   :: k #$\in$# ks -> JMap ks k v -> v

reinsert
  :: k #$\in$# ks -> v -> JMap ks k v -> JMap ks k v

withMap
:: Map k v  -> (forall s. JMap s k v -> t) -> t
