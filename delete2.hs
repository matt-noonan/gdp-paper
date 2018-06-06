subset_elts :: (a #$\subseteq$# b) -> (x #$\in$# a) -> Proof (x #$\in$# b)
subset_elts _ = axiom -- or import from a theory library

insert_supset :: Proof (Keys m #$\subseteq$# Keys (Insert k v m))
insert_supset = axiom

key_is_present :: Proof (k #$\in$# Insert k v m)
key_is_present = axiom

delete_subset :: Proof (Keys (Delete k m) #$\subseteq$# Keys m)
delete_subset = axiom

key_is_missing  :: (k' #$\in$# Keys m)
                -> Not (k == k')
                -> Proof (k' #$\in$# Keys (Delete k m))
key_is_missing _ _ = axiom
