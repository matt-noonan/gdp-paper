subset_elts :: (a #$\subseteq$# b) -> (x #$\in$# a)
            -> Proof (x #$\in$# b)
subset_elts _ = sorry

insert_supset :: Proof (Keys m #$\subseteq$# Keys (Insert k v m))
insert_supset = sorry

key_is_present :: Proof (k #$\in$# Insert k v m)
key_is_present = sorry

delete_subset :: Proof (Keys (Delete k m) #$\subseteq$# Keys m)
delete_subset = sorry

key_is_missing  :: (k' #$\in$# Keys m) -> (k /= k')
                -> Proof (k' #$\in$# Keys (Delete k m))
key_is_missing _ _ = sorry
