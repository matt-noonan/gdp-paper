newtype Insert k v m = Insert Defn
newtype Delete k m = Delete Defn

insert :: Ord k
       => (k ~~ k)
       -> (v ~~ v)
       -> (Map k v ~~ m)
       -> (Map k v ~~ Insert k v m)
insert k v m = defn (Map.insert (bare k) (bare v) (bare m))

delete :: Ord k
       => (k ~~ k ::: k #$\in$# KeySet m)
       -> (Map k v ~~ m)
       -> (Map k v ~~ Delete k m)
delete k m = defn (Map.delete (bare k) (bare m))

subset_elts :: (a #$\subseteq$# b) -> (x #$\in$# a) -> Proof (x #$\in$# b)
subset_elts _ = sorry

insert_supset :: Proof (Keys m #$\subseteq$# Keys (Insert k v m))
insert_supset = sorry

key_is_present :: Proof (k #$\in$# Insert k v m)
key_is_present = sorry

delete_subset :: Proof (Keys (Delete k m) #$\subseteq$# Keys m)
delete_subset = sorry

only_k_missing  :: (k' #$\in$# Keys m) -> (k /= k')
                -> Proof (k' #$\in$# Keys (Delete k m))
only_k_missing _ _ = sorry
