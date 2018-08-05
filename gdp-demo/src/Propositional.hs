{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Propositional
  ( -- * The `Proof` type
    Proof
  , (|.), (|$), (|/), (|\)
  , axiom
  , sorry
  
  -- * Refinement types
  , type (:::), (...), type (?), assert
  -- * First-order Logic

  -- ** Logical symbols
  , TRUE, FALSE
  
  , And, type (&&)
  , Or,  type (||)
  , Impl, type (-->)
  , Not
  , ForAll
  , Exists

  -- ** Natural deduction

  -- *** Tautologies
  , true
  , noncontra

  -- *** Introduction rules

  -- | Introduction rules give the elementary building blocks
  --   for creating a formula from simpler ones.
  , and_intro
  , or_introL
  , or_introR
  , impl_intro
  , not_intro
  , contrapositive
  , contradicts
  , contradicts'
  , univ_intro
  , ex_intro

  -- *** Elimination rules

  -- | Elimination rules give the elementary building blocks for
  --   decomposing a complex formula into simpler ones.
  , and_elimL
  , and_elimR
  , and_elim
  , or_elim
  , or_elimL
  , or_elimR
  , impl_elim
  , modus_ponens
  , modus_tollens
  , absurd
  , univ_elim
  , ex_elim

  -- *** Classical logic and the Law of the Excluded Middle
  , Classical
  , lem

  -- * Algebraic properties
  , Reflexive(..)
  , Irreflexive(..)
  , Symmetric(..)
  , Antisymmetric(..)
  , Transitive(..)
  , transitive'

  , Idempotent(..)
  , Commutative(..)
  , Associative(..)
  , Distributive(..)

  , Injective(..)

  ) where

import The
import Named
import Arguments

import Data.Coerce
import Control.Monad ((>=>))

{--------------------------------------------------
  The `Proof` monad
--------------------------------------------------}

{-| The @Proof@ monad is used as a domain-specific
    language for constructing proofs. A value of type
    @Proof p@ represents a proof of the proposition @p@.

    For example, this function constructions a proof of
    "P or Q" from the assumption "P and Q":

> and2or :: (p && q) -> Proof (p || q)
>
> and2or pq = and_elimL pq |$ or_introL

    If the body of the proof does not match the proposition
    you claim to be proving, the compiler will raise a type
    error. Here, we accidentally try to use @or_introR@
    instead of @or_introL@:

> and2or :: (p && q) -> Proof (p || q)
>
> and2or pq = and_elimL pq |$ or_introR

resulting in the error

@
    • Couldn't match type ‘p’ with ‘q’
      ‘p’ is a rigid type variable bound by
        the type signature for:
          and2or :: forall p q. Proof (p && q) -> Proof (p || q)

      ‘q’ is a rigid type variable bound by
        the type signature for:
          and2or :: forall p q. PRoof (p && q) -> Proof (p || q)

      Expected type: Proof (p || q)
        Actual type: Proof (p || p)
@
-}
data Proof (pf :: *) = QED

{-| This operator is just a specialization of @flip ($)@, but
    can be used to create nicely delineated chains of
    derivations within a larger proof. The first statement
    in the chain should produce a formula; @(|$)@ then
    pipes this formula into the following derivation rule.

> and2or :: (p && q) -> Proof (p || q)
>
> and2or pq =  and_elimL pq
>           |$ or_introL
-}

(|$) :: a -> (a -> b) -> b
(|$) = flip ($)

infixr 7 |$

--(|-) :: ((p -> Proof r) -> Proof r) -> (p -> Proof r) -> Proof r

{-| This operator is used to create nicely delineated chains of
    derivations within a larger proof. If X and Y are two
    deduction rules, each with a single premise and a single
    conclusion, and the premise of Y matches the conclusion of X,
    then @X |. Y@ represents the composition of the two
    deduction rules.

> and2or :: (p && q) -> Proof (p || q)
>
> and2or =  and_elimL
>        |. or_introL
-}

(|.) :: (a -> b) -> (b -> c) -> (a -> c)
(|.) = flip (.)

infixr 9 |.

{-| The @(|/)@ operator is used to feed the remainder of the proof in
    as a premise of the first argument.

    A common use-case is with the @Or@-elimination rules @or_elimL@ and
    @or_elimR@, when one case is trivial. For example, suppose we wanted
    to prove that @R && (P or (Q and (Q implies P)))@ implies @P@:

@
my_proof :: r && (p || (q && (q --> p))) -> Proof p

my_proof =
  do  and_elimR          -- Forget the irrelevant r.
   |. or_elimL id        -- The first case of the || is immediate;
   |/ and_elim           -- The rest of the proof handles the second case,
   |. uncurry impl_elim  --   by unpacking the && and feeding the q into
                         --   the implication (q --> p).
@

    The rising @/@ is meant to suggest the bottom half of the proof getting
    plugged in to the Or-elimination line.
-}
(|/) :: (a -> (b -> c) -> c) -> (b -> c) -> (a -> c)
(|/) = flip
infixr 9 |/

my_proof' :: Proof (r && (p || (q && (q --> p)))) -> Proof p

my_proof' =  and_elimR
          |. or_elimL id
          |/ (uncurry impl_elim . and_elim)

{-| The @(|\\)@ operator is used to take the subproof so far and feed it
    into a rule that is expecting a subproof as a premise.

    A common use-case is with the @Not@-introduction rule @not_intro@,
    which has a type that fits the second argument of @(|\\)@. By way
    of example, here is a proof that "P implies Q" along with "Not Q"
    implies "Not P".

@
my_proof :: (p --> q) -> (Not p --> r) -> Not q -> Proof r

my_proof impl1 impl2 q' =
      modus_ponens impl1   -- This line, composed with the next,
   |. contradicts' q'      --   form a proof of FALSE from p.
   |\\ not_intro            -- Closing the subproof above, conclude not-p.
   |. modus_ponens impl2   -- Now apply the second implication to conclude r.
@

    The falling @\\@ is meant to suggest the upper half of the proof
    being closed off by the Not-introduction line.
-}
(|\) :: (a -> b) -> ((a -> b) -> c) -> c
(|\) = flip ($)
infixl 8 |\

my_proof :: Proof (p --> q) -> Proof (Not p --> r) -> Proof (Not q) -> Proof r

my_proof impl1 impl2 q' =
     modus_ponens impl1    -- This line, composed with the next,           Pf p -> Pf q
  |. contradicts' q'       --   form a proof of FALSE from p.              Pf p -> Pf FALSE
  |\ not_intro             -- Closing the subproof above, conclude not-p.  (Pf p -> Pf FALSE) -> Pf !p
  |. modus_ponens impl2                                               --   Pf !p -> Pf r

-- | @axiom@ can be used to provide a "proof" of
--   any proposition, by simply assering it as
--   true. This is useful for stubbing out portions
--   of a proof as you work on it, but subverts
--   the entire proof system.
--
-- _Completed proofs should never use @axiom@!_
axiom :: Proof p
axiom = QED

-- | @axiom@ can be used to provide a "proof" of
--   any proposition, by simply assering it as
--   true. This is useful for stubbing out portions
--   of a proof as you work on it, but subverts
--   the entire proof system.
--
-- _Completed proofs should never use @axiom@!_
{-# WARNING sorry "`sorry` remains in program." #-}
sorry :: Proof p
sorry = QED

newtype TRUE  = TRUE Defn
newtype FALSE = FALSE Defn
newtype And p q = And Defn
newtype Or p q  = Or  Defn
newtype Not p   = Not Defn
newtype Impl p q = Impl Defn
newtype Exists x p = Exists Defn
newtype ForAll x p = ForAll Defn

type p || q  = p `Or` q
type p && q  = p `And` q
type p --> q = p `Impl` q

infixl 2 `Or`
infixl 2 ||

infixl 3 `And`
infixl 3 &&

infixr 1 `Impl`
infixr 1 -->

{--------------------------------------------------
  Refinement types
--------------------------------------------------}

{-| Given a type @a@ and a proposition @p@, the
    type @(a ::: p)@ represents a /refinement type/
    for @a@. Values of type @(a ::: p)@ are values
    of type @a@ such that proposition @p@ holds.

    Values of the refinement type @(a ::: p)@ have
    the same run-time representation as values of
    the normal type @a@; the proposition @p@ does
    not carry a run-time space or time cost.

    By design, there are no functions to extract
    the @p@ from a @(a ::: p)@.
-}
newtype a ::: p = SuchThat a
infixr 1 :::

instance The (a ~~ n ::: p) a where
  the (SuchThat x) = the x

newtype a ? (p :: * -> *) = Satisfies a
infixr 1 ?

instance The (a ? p) a

-- | Re-introduce a name for a refined value.
rename :: (a ? p) -> (forall name. (a ~~ name ::: p name) -> t) -> t
rename x k = name (the x) (\x -> k (x ...axiom))

assert :: Defining (p ()) => a -> (a ? p)
assert = coerce

(...) :: a -> Proof p -> (a ::: p)
x ...proof = coerce x

{--------------------------------------------------
  Tautologies
--------------------------------------------------}

-- | @TRUE@ is always true, and can be introduced into a proof at any time.
true :: Proof TRUE
true = QED

-- | The Law of Noncontradiction: for any proposition P, "P and not-P" is false.
noncontra :: Proof (Not (p && Not p))
noncontra = QED

{--------------------------------------------------
  Introduction rules
--------------------------------------------------}

-- | Prove "P and Q" from P and Q.
and_intro :: Proof p -> Proof q -> Proof (p && q)
and_intro _ _ = QED

-- | Prove "P and Q" from Q and P.
and_intro' :: Proof q -> Proof p -> Proof (p && q)
and_intro' _ _ = QED

-- | Prove "P or Q" from  P.
or_introL :: Proof p -> Proof (p || q)
or_introL _ = QED

-- | Prove "P or Q" from Q.
or_introR :: Proof q -> Proof (p || q)
or_introR _ = QED

-- | Prove "P implies Q" by demonstrating that,
--   from the assumption P, you can prove Q.
impl_intro :: (Proof p -> Proof q) -> Proof (p --> q)
impl_intro _ = QED

-- | Prove "not P" by demonstrating that,
--   from the assumption P, you can derive a false conclusion.
not_intro :: (Proof p -> Proof FALSE) -> Proof (Not p)
not_intro _ = QED

-- | `contrapositive` is an alias for `not_intro`, with
--   a somewhat more suggestive name. Not-introduction
--   corresponds to the proof technique "proof by contrapositive".
contrapositive :: (Proof p -> Proof FALSE) -> Proof (Not p)
contrapositive = not_intro

-- | Prove a contradiction from "P" and "not P".
contradicts :: Proof p -> Proof (Not p) -> Proof FALSE
contradicts _ _ = QED

-- | `contradicts'` is `contradicts` with the arguments
--   flipped. It is useful when you want to partially
--   apply `contradicts` to a negation.
contradicts' :: Proof (Not p) -> Proof p -> Proof FALSE
contradicts' = flip contradicts

-- | Prove "For all x, P(x)" from a proof of P(*c*) with
--   *c* indeterminate.
univ_intro :: (forall c. Proof (p c)) -> Proof (ForAll x (p x))
univ_intro _ = QED

-- | Prove "There exists an x such that P(x)" from a specific
--   instance of P(c).
ex_intro :: Proof (p c) -> Proof (Exists x (p x))
ex_intro _ = QED

{--------------------------------------------------
  Elimination rules
--------------------------------------------------}

-- | From the assumption "P and Q", produce a proof of P.
and_elimL :: Proof (p && q) -> Proof p
and_elimL = fst . and_elim

-- | From the assumption "P and Q", produce a proof of Q.
and_elimR :: Proof (p && q) -> Proof q
and_elimR = snd . and_elim

-- | From the assumption "P and Q", produce both a proof
--   of P, and a proof of Q.
and_elim :: Proof (p && q) -> (Proof p, Proof q)
and_elim _ = (QED, QED)
  
{-| If you have a proof of R from P, and a proof of
     R from Q, then convert "P or Q" into a proof of R.
-}
or_elim :: (Proof p -> Proof r) -> (Proof q -> Proof r) -> Proof (p || q) -> Proof r
or_elim _ _ _ = QED

{-| Eliminate the first alternative in a conjunction.
-}
or_elimL :: (Proof p -> Proof r) -> Proof (p || q) -> (Proof q -> Proof r) -> Proof r
or_elimL case1 disj case2 = or_elim case1 case2 disj

{-| Eliminate the second alternative in a conjunction.
-}
or_elimR :: (Proof q -> Proof r) -> Proof (p || q) -> (Proof p -> Proof r) -> Proof r
or_elimR case2 disj case1 = or_elim case1 case2 disj

-- | Given "P imples Q" and P, produce a proof of Q.
--   (modus ponens)
impl_elim :: Proof p -> Proof (p --> q) -> Proof q
impl_elim _ _ = QED

-- | @modus_ponens@ is just @impl_elim@ with the arguments
--   flipped. In this form, it is useful for partially
--   applying an implication.
modus_ponens :: Proof (p --> q) -> Proof p -> Proof q
modus_ponens = flip impl_elim

{-| Modus tollens lets you prove "Not P" from
    "P implies Q" and "Not Q".

    Modus tollens is not fundamental, and can be derived from
    other rules:

@
                                  -----         (assumption)
                        p --> q,    p
                       ---------------------    (modus ponens)
                 q,           Not q    
              --------------------------        (contradicts')
                      FALSE
          ------------------------------------- (not-intro)
                             Not p
@

We can encode this proof tree more-or-less directly as a @Proof@
to implement @modus_tollens@:

@
modus_tollens :: (p --> q) -> Not q -> Proof (Not p)

modus_tollens impl q' =
  do  modus_ponens impl
   |. contradicts' q'
   |\\ not_intro
@
-}

modus_tollens :: Proof (p --> q) -> Proof (Not q) -> Proof (Not p)
modus_tollens impl q' =
  do  modus_ponens impl
   |. contradicts' q'
   |\ not_intro

-- | From a falsity, prove anything.
absurd :: Proof FALSE -> Proof p
absurd _ = QED

-- | Given "For all x, P(x)" and any c, prove the proposition
--   "P(c)".
univ_elim :: Proof (ForAll x (p x)) -> Proof (p c)
univ_elim _ = QED

-- | Given a proof of Q from P(c) with c generic, and the
--   statement "There exists an x such that P(x)", produce
--   a proof of Q.
ex_elim :: (forall c. Proof (p c) -> Proof q) -> Proof (Exists x (p x)) -> Proof q
ex_elim _ _ = QED


{--------------------------------------------------
  Classical logic
--------------------------------------------------}

-- | The inference rules so far have all been valid in
--   constructive logic. Proofs in classical logic are
--   also allowed, but will be constrained by the
--   `Classical` typeclass.
class Classical

-- | The Law of the Excluded Middle: for any proposition
--   P, assert that either P is true, or Not P is true.
lem :: Classical => Proof (p || Not p)
lem = QED

{-| Proof by contradiction: this proof technique allows
     you to prove P by showing that, from "Not P", you
     can prove a falsehood.
  
     Proof by contradiction is not a theorem of
     constructive logic, so it requires the @Classical@
     constraint. But note that the similar technique
     of proof by contrapositive (not-introduction) /is/
     valid in constructive logic! For comparison, the two types are:

@
contradiction  :: Classical => (Not p -> Proof FALSE) -> p
not_intro      ::              (p     -> Proof FALSE) -> Not p
@

The derivation of proof by contradiction from the law of the excluded
middle goes like this: first, use the law of the excluded middle to
prove @p || Not p@. Then use or-elimination to prove @p@ for each
alternative. The first alternative is immediate; for the second
alternative, use the provided implication to get a proof of falsehood,
from which the desired conclusion is derived.

@
contradiction impl =
  do  lem             -- introduce p || Not p
   |$ or_elimL id     -- dispatch the first, straightforward case
   |/ impl            -- Now we're on the second case. Apply the implication..
   |. absurd          -- .. and, from falsity, conclude p.
@
-}
contradiction :: forall p. Classical => (Proof (Not p) -> Proof FALSE) -> Proof p
contradiction impl =
  do  lem
   |$ or_elimL id
   |/ impl
   |. absurd
  
{-| Double-negation elimination. This is another non-constructive
    proof technique, so it requires the @Classical@ constraints.

    The derivation of double-negation elimination follows from
    proof by contradiction, since "Not (Not p)" contradicts "Not p".
@
not_not_elim p'' = contradiction (contradicts' p'')
@
-}

not_not_elim :: Classical => Proof (Not (Not p)) -> Proof p
not_not_elim p'' = contradiction (contradicts' p'')

{--------------------------------------------------------
  Special properties of predicates and functions
--------------------------------------------------------}

{-| A binary relation R is /reflexive/ if, for all x,
    R(x, x) is true. The @Reflexive r@ typeclass provides
    a single method, @refl :: Proof (r x x)@,
    proving R(x, x) for an arbitrary x.

    Within the module where the relation @R@ is defined, you can
    declare @R@ to be reflexive with an empty instance:

@
-- Define a reflexive binary relation
newtype SameColor p q = SameColor Defn
instance Reflexive SameColor
@
-}   
class Reflexive r where
  refl :: Proof (r x x)
  default refl :: (Defining (r x x)) => Proof (r x x)
  refl = QED

class Irreflexive r where
  irrefl :: Proof (Not (r x x))
  default irrefl :: (Defining (r x x)) => Proof (Not (r x x))
  irrefl = QED

{-| A binary relation R is /symmetric/ if, for all x and y,
    R(x, y) is true if and only if R(y, x) is true. The
    @Symmetric@ typeclass provides
    a single method, @symmetric :: r x y -> Proof (r y x)@,
    proving the implication "R(x, y) implies R(y, x)".

    Within the module where @R@ is defined, you can
    declare @R@ to be symmetric with an empty instance:

@
-- Define a symmetric binary relation
newtype NextTo p q = NextTo Defn
instance Symmetric NextTo
@
-}   
class Symmetric c where
  symmetric :: c p q -> Proof (c q p)
  default symmetric :: (Defining (c p q)) => c p q -> Proof (c q p)
  symmetric _ = QED

class Antisymmetric c where
  antisymmetric :: c p q -> Proof (Not (c q p))
  default antisymmetric :: Defining (c p q) => c p q -> Proof (Not (c q p))
  antisymmetric _ = QED
  
  antisymmetric' :: Not (c p q) -> Proof (c q p)
  default antisymmetric' :: Defining (c p q) => Not (c p q) -> Proof (c q p)
  antisymmetric' _ = QED

  
{-| A binary relation R is /transitive/ if, for all x, y, z,
    if R(x, y) is true and R(y, z) is true, then  R(x, z) is true.
    The @Transitive r@ typeclass provides
    a single method, @transitive :: r x y -> r y z -> Proof (r x z)@,
    proving R(x, z) from R(x, y) and R(y, z).

    Within the module where @R@ is defined, you can
    declare @R@ to be transitive with an empty instance:

@
-- Define a transitive binary relation
newtype CanReach p q = CanReach Defn
instance Transitive CanReach
@
-}   
class Transitive c where
  transitive :: Proof (c p q) -> Proof (c q r) -> Proof (c p r)
  default transitive :: Defining (c p q) => Proof (c p q) -> Proof (c q r) -> Proof (c p r)
  transitive _ _ = QED

transitive' :: Transitive c => Proof (c q r) -> Proof (c p q) -> Proof (c p r)
transitive' = flip transitive

class Idempotent c where
  idempotent :: Proof (c p p == p)
  default idempotent :: Defining (c p p) => Proof (c p p == p)
  idempotent = QED
  
class Commutative c where
  
  commutative :: Proof (c p q == c q p)
  default commutative :: Defining (c p q) => Proof (c p q == c q p)
  commutative = QED
  
class Associative c where

  associative :: Proof (c p (c q r) == c (c p q) r)
  default associative :: Defining (c p q) => Proof (c p (c q r) == c (c p q) r)
  associative = QED
  

class Distributive c c' where

  distributive :: Proof (c p (c' q r) == c' (c p q) (c p r))
  default distributive :: (Defining (c p q), Defining (c' p q)) => Proof (c p (c' q r) == c' (c p q) (c p r))
  distributive = QED

class Injective c where
  elim_inj :: Proof (c (f x) == c (f y)) -> Proof (c x == c y)
  default elim_inj :: (Defining (c (f x)), Defining (c (f y)), Defining (c x), Defining (c y))
   => Proof (c (f x) == c (f y)) -> Proof (c x == c y)
  elim_inj _ = QED

{--------------------------------------------------
  Algebraic properties
--------------------------------------------------}

instance Symmetric And
instance Symmetric Or

instance Associative And
instance Associative Or

instance Distributive And And
instance Distributive And Or
instance Distributive Or  And
instance Distributive Or  Or

-- | The @Equals@ relation is used to express equality between two entities.
--   Given an equality, you are then able to substitute one side of the equality
--   for the other, anywhere you please.
newtype Equals x y = Equals Defn
type x == y = x `Equals` y
infix 4 ==
  
instance Reflexive   Equals
instance Symmetric   Equals
instance Transitive  Equals

instance Argument (Equals x y) 0 where
  type GetArg (Equals x y) 0    = x
  type SetArg (Equals x y) 0 x' = Equals x' y

instance Argument (Equals x y) 1 where
  type GetArg (Equals x y) 1    = y
  type SetArg (Equals x y) 1 y' = Equals x y'
  
-- | @x /= y@ is a convenient alias for @Not (x `Equals` y)@.
type x /= y = Not (x == y)
infix 4 /=

-- | A `Lawful c t` instance asserts that the typeclass `c t`
--   obeys the expected laws of `c` (whatever that means).
--   See the `same` function for an application.
class c t => Lawful c t

instance Lawful Eq Int
instance Lawful Eq a => Lawful Eq [a]

-- | If `Eq a` is a lawful instance, compare two named `a`s and, if they
--   are equal, reflect that equality into an equality of names.
same :: Lawful Eq a => (a ~~ x) -> (a ~~ y) -> Maybe (a ~~ x ::: x == y)
same x y = if the x == the y
             then Just (coerce x)
             else Nothing

-- | Given a function and an equality over ones of its arguments,
--   replace the left-hand side of the equality with the right-hand side.
substitute :: (Argument f n, GetArg f n ~ x)
    => Arg n -> Proof (x == x') -> Proof (f == SetArg f n x')
substitute _ _ = QED

-- | Substitute @x'@ for @x@ under the function @f@, on the left-hand side
--   of an equality.
substituteL :: (Argument f n, GetArg f n ~ x)
    => Arg n -> Proof (x == x') -> Proof (f == g) -> Proof (SetArg f n x' == g)
substituteL _ _ _ = QED

-- | Substitute @x'@ for @x@ under the function @f@, on the right-hand side
--   of an equality.
substituteR :: (Argument f n, GetArg f n ~ x)
    => Arg n -> Proof (x == x') -> Proof (g == f) -> Proof (g == SetArg f n x')
substituteR _ _ _ = QED

-- | Apply a function to both sides of an equality.
apply :: forall f n x x'. (Argument f n, GetArg f n ~ x)
    => Arg n -> Proof (x == x') -> Proof (f == SetArg f n x')
apply _ _ = QED
  
