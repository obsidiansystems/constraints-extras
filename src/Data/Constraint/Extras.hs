{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Throughout this module, we use the following GADT and @ArgDict@ instance
-- in our examples:
--
-- > {-# LANGUAGE StandaloneDeriving #-}
-- >
-- > data Tag a where
-- >   I :: Tag Int
-- >   B :: Tag Bool
-- > deriving instance Show (Tag a)
-- >
-- > $(deriveArgDict ''Tag)
--
-- The constructors of @Tag@ mean that a type variable @a@ in @Tag a@ must
-- come from the set { @Int@, @Bool@}. We call this the "set of types @a@ that
-- could be applied to @Tag@".
module Data.Constraint.Extras
  ( -- * The ArgDict typeclass
    ArgDict(..)
  , ConstraintsFor'
  , argDict'
  , ConstraintsForV
  , argDictV
    -- * Bringing instances into scope
  , Has
  , has
  , Has'
  , has'
  , HasV
  , hasV
  , whichever
    -- * Misc
  , Implies1(..)
    -- * Deprecated
  , ArgDictV
  ) where

import Data.Constraint
import Data.Constraint.Compose
import Data.Constraint.Flip
import Data.Constraint.Forall
import Data.Kind

-- | Morally, this class is for GADTs whose indices can be finitely
-- enumerated. An @'ArgDict' c f@ instance allows us to do two things:
--
-- 1. 'ConstraintsFor' requests the set of constraints @c a@ for all
--    possible types @a@ that could be applied to @f@.
--
-- 2. 'argDict' selects a specific @c a@ given a value of type @f a@.
--
-- Use 'Data.Constraint.Extras.TH.deriveArgDict' to derive instances
-- of this class.
class ArgDict (c :: k -> Constraint) (f :: k -> Type) where
  -- | Apply @c@ to each possible type @a@ that could appear in a @f a@.
  --
  -- > ConstraintsFor Show Tag = (Show Int, Show Bool)
  type ConstraintsFor f c :: Constraint

  -- | Use an @f a@ to select a specific dictionary from @ConstraintsFor f c@.
  --
  -- > argDict I :: Dict (Show Int)
  argDict :: ConstraintsFor f c => f a -> Dict (c a)

-- | \"Primed\" variants (@ConstraintsFor'@, 'argDict'', 'Has'',
-- 'has'', &c.) use the 'ArgDict' instance on @f@ to apply constraints
-- on @g a@ instead of just @a@. This is often useful when you have
-- data structures parameterised by something of kind @(x -> Type) ->
-- Type@, like in the @dependent-sum@ and @dependent-map@ libraries.
--
-- > ConstraintsFor' Tag Show Identity = (Show (Identity Int), Show (Identity Bool)
type ConstraintsFor' f (c :: k -> Constraint) (g :: k' -> k) = ConstraintsFor f (ComposeC c g)

-- | Get a dictionary for a specific @g a@, using a value of type @f a@.
--
-- > argDict' B :: Dict (Show (Identity Bool))
argDict' :: forall f c g a. (Has' c f g) => f a -> Dict (c (g a))
argDict' tag = case argDict tag of
  (Dict :: Dict (ComposeC c g a)) -> Dict

type ConstraintsForV (f :: (k -> k') -> Type) (c :: k' -> Constraint) (g :: k) = ConstraintsFor f (FlipC (ComposeC c) g)

argDictV :: forall f c g v. (HasV c f g) => f v -> Dict (c (v g))
argDictV tag = case argDict tag of
  (Dict :: Dict (FlipC (ComposeC c) g a)) -> Dict

{-# DEPRECATED ArgDictV "Just use 'ArgDict'" #-}
type ArgDictV f c = ArgDict f c

-- | @Has c f@ is a constraint which means that for every type @a@
-- that could be applied to @f@, we have @c a@.
--
-- > Has Show Tag = (ArgDict Show Tag, Show Int, Show Bool)
type Has (c :: k -> Constraint) f = (ArgDict c f, ConstraintsFor f c)

-- | @Has' c f g@ is a constraint which means that for every type @a@
-- that could be applied to @f@, we have @c (g a)@.
--
-- > Has' Show Tag Identity = (ArgDict (Show . Identity) Tag, Show (Identity Int), Show (Identity Bool))
type Has' (c :: k -> Constraint) f (g :: k' -> k) = (ArgDict (ComposeC c g) f, ConstraintsFor' f c g)
type HasV c f g = (ArgDict (FlipC (ComposeC c) g) f, ConstraintsForV f c g)

-- | Use the @a@ from @f a@ to select a specific @c a@ constraint, and
-- bring it into scope. The order of type variables is chosen to work
-- with @-XTypeApplications@.
--
-- > -- Hold an a, along with a tag identifying the a.
-- > data SomeTagged tag where
-- >   SomeTagged :: a -> tag a -> SomeTagged tag
-- >
-- > -- Use the stored tag to identify the thing we have, allowing us to call 'show'. Note that we
-- > -- have no knowledge of the tag type.
-- > showSomeTagged :: Has Show tag => SomeTagged tag -> String
-- > showSomeTagged (SomeTagged a tag) = has @Show tag $ show a
has :: forall c f a r. Has c f => f a -> (c a => r) -> r
has k r | (Dict :: Dict (c a)) <- argDict k = r

-- | Like 'has', but we get a @c (g a)@ instance brought into scope
-- instead. Use @-XTypeApplications@ to specify @c@ and @g@.
--
-- > -- From dependent-sum:Data.Dependent.Sum
-- > data DSum tag f = forall a. !(tag a) :=> f a
-- >
-- > -- Show the value from a dependent sum. (We'll need 'whichever', discussed later, to show the key.)
-- > showDSumVal :: forall tag f . Has' Show tag f => DSum tag f -> String
-- > showDSumVal (tag :=> fa) = has' @Show @f tag $ show fa
has' :: forall c g f a r. Has' c f g => f a -> (c (g a) => r) -> r
has' k r | (Dict :: Dict (c (g a))) <- argDict' k = r

hasV :: forall c g f v r. HasV c f g => f v -> (c (v g) => r) -> r
hasV k r | (Dict :: Dict (c (v g))) <- argDictV k = r

-- | Given "forall a. @c (t a)@" (the @ForallF c t@ constraint), select a
-- specific @a@, and bring @c (t a)@ into scope. Use @-XTypeApplications@ to
-- specify @c@, @t@ and @a@.
--
-- > -- Show the tag of a dependent sum, even though we don't know the tag type.
-- > showDSumKey :: forall tag f . ForallF Show tag => DSum tag f -> String
-- > showDSumKey ((tag :: tag a) :=> fa) = whichever @Show @tag @a $ show tag
whichever :: forall c t a r. ForallF c t => (c (t a) => r) -> r
whichever r = r \\ (instF :: ForallF c t :- c (t a))

-- | Allows explicit specification of constraint implication.
class Implies1 c d where
  implies1 :: c a :- d a
