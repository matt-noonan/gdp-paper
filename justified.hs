newtype JMap ks k v = JMap (Map k v) deriving Functor
newtype Key  ks k   = Key k

instance The (JMap ks k v) (Map k v)
instance The (Key ks k) k

member   :: k -> JMap ks k v -> Maybe (Key ks k)
lookup   :: Key ks k -> JMap ks k v -> v
reinsert :: Key ks k -> v -> JMap ks k v -> JMap ks k v
withMap  :: Map k v -> (forall ks. JMap ks k v -> t) -> t
