newtype Insert k v m = Insert Defn
newtype Delete k m   = Delete Defn

insert :: Ord k
       => (k ~~ k)
       -> (v ~~ v)
       -> (Map k v ~~ m)
       -> (Map k v ~~ Insert k v m)
insert k v m =
    defn (Map.insert (bare k) (bare v) (bare m))

delete :: Ord k
       => (k ~~ k ::: k #$\in$# KeySet m)
       -> (Map k v ~~ m)
       -> (Map k v ~~ Delete k m)
delete k m = defn (Map.delete (bare k) (bare m))
