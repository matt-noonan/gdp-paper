newtype JMap φ k v = JMap (Map k v)
    deriving Functor

newtype JKey φ k = Element k

instance The (JMap φ k v) (Map k v)
instance The (JKey φ k)  k

member ::  k -> JMap φ k v -> Maybe (JKey φ k)

lookup   :: JKey φ k -> JMap φ k v -> v

reinsert
  :: JKey φ k -> v -> JMap φ k v -> JMap φ k v

withMap
  :: Map k v  -> (forall φ. JMap φ k v -> t) -> t
