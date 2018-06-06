-- Type exported, constructor hidden (but see `axiom`)
data Proof p = QED deriving (Functor, Applicative, Monad)

-- Attaching predicates to values
newtype a ::: p = SuchThat Defn

(...) :: a -> Proof p -> (a ::: p)
x ...proof = coerce x

-- Logical constants
data TRUE
data FALSE
data p && q
data p || q
data p --> q
data Not p
data p == q

-- Local inference rules (implementations all
-- ignore parameters and return `QED`)
and_intro   :: p     ->   q       -> Proof (p && q)
or_elimL    :: (p || q)           -> Proof p
impl_intro  :: (p -> Proof q)     -> Proof (p --> q)
impl_elim   :: (p --> q)   ->  p  -> Proof q
not_intro   :: (p -> Proof FALSE) -> Proof (Not p)
contradicts :: p     ->   Not p   -> Proof FALSE
absurd      :: FALSE              -> Proof p
  -- ... and many more

-- Proof combinators (specialized from Control.Monad)
-- Careful fixity definitions allow easy composition
-- of linear proofs.
(|$) :: p -> (p -> Proof q) -> Proof q
(|.) :: (p -> Proof q)
     -> (q -> Proof r) -> (p -> Proof r)
(|/) :: ((p -> Proof q) -> Proof r)
     -> (p -> Proof q) -> Proof r
(|\) :: (p -> Proof q)
     -> ((p -> Proof q) -> Proof r) -> Proof r

-- Exported function that allows library authors to
-- assert arbitrary axioms about their API.
axiom :: Proof p
axiom = QED
