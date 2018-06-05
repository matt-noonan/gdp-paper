-- Type exported, constructor hidden (but see `sorry`)
data Proof p = QED deriving (Functor, Applicative, Monad)

-- Attaching predicates to values
newtype a ::: p = SuchThat Defn

because :: a -> Proof p -> (a ::: p)
x `because` p = coerce x

-- Logical constants
data TRUE
data FALSE
data p && q
data p || q
data p --> q
data Not p

-- Local inference rules (implementations all
-- ignore parameters and return `QED`)
and_intro   :: p -> q -> Proof (p && q)
or_elimL    :: (p || q) -> Proof p
impl_intro  :: (p -> Proof q) -> Proof (p --> q)
contradicts :: p -> Not p -> Proof FALSE
absurd      :: FALSE -> Proof p
  -- ... and many more

-- Proof combinators (specialized from Control.Monad)
-- Carefuly fixity definitions allow easy composition
-- of linear proofs.
(|$) :: p -> (p -> Proof q) -> Proof q
(|.) :: (p -> Proof q)
     -> (q -> Proof r) -> (p -> Proof r)
(|/) :: ((p -> Proof q) -> Proof r)
     -> (p -> Proof q) -> Proof r
(|\) :: (p -> Proof q)
     -> ((p -> Proof q) -> Proof r) -> Proof r

-- Exported function that allowis library authors to
-- sassert arbitrary axioms about their API.
sorry :: Proof p
sorry = QED
