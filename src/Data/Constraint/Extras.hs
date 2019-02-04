{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Constraint.Extras where

import Data.Constraint
import Data.Constraint.Compose
import Data.Constraint.Forall

-- | Morally, this class is for GADTs whose indices can be finitely enumerated. It provides operations which will
-- select the appropriate type class dictionary from among a list of contenders based on a value of the type.
-- There are a few different variations of this which we'd like to be able to support, and they're all implemented
-- in the same fashion at the term level, by pattern matching on the constructors of the GADT, and producing Dict
-- as the result.
-- It would be nice to have some way to stop the proliferation of these variants and unify the existing ones, but
-- at the moment, it appears to require honest type level functions. (Closed type families which must be fully
-- applied didn't quite cut it when I tried). Some symbolic type-level application could do the trick, but I didn't
-- want to go quite that far at the time of writing.
class ArgDict f where
  type ConstraintsFor f (c :: k -> Constraint) :: Constraint
  argDict :: ConstraintsFor f c => f a -> Dict (c a)

type ConstraintsFor' f (c :: k -> Constraint) (g :: k' -> k) = ConstraintsFor f (ComposeC c g)

argDict' :: forall f c g a. (ArgDict f, ConstraintsFor' f c g) => f a -> Dict (c (g a))
argDict' tag = case argDict tag of
  (Dict :: Dict (ComposeC c g a)) -> Dict

class c h g => FlipC (c :: k -> k' -> Constraint) (g :: k') (h :: k)
instance c h g => FlipC c g h

type ConstraintsForV (f :: (k -> k') -> *) (c :: k' -> Constraint) (g :: k) = ConstraintsFor f (FlipC (ComposeC c) g)

argDictV :: forall f c g v. (ArgDict f, ConstraintsForV f c g) => f v -> Dict (c (v g))
argDictV tag = case argDict tag of
  (Dict :: Dict (FlipC (ComposeC c) g a)) -> Dict

{-# DEPRECATED ArgDictV "Just use 'ArgDict'" #-}
type ArgDictV f = ArgDict f

type Has (c :: k -> Constraint) f = (ArgDict f, ConstraintsFor f c)
type Has' (c :: k -> Constraint) f (g :: k' -> k) = (ArgDict f, ConstraintsFor' f c g)
type HasV c f g = (ArgDict f, ConstraintsForV f c g)

has :: forall c f a r. (Has c f) => f a -> (c a => r) -> r
has k r | (Dict :: Dict (c a)) <- argDict k = r

has' :: forall c g f a r. (Has' c f g) => f a -> (c (g a) => r) -> r
has' k r | (Dict :: Dict (c (g a))) <- argDict' k = r

hasV :: forall c g f v r. (HasV c f g) => f v -> (c (v g) => r) -> r
hasV k r | (Dict :: Dict (c (v g))) <- argDictV k = r

whichever :: forall c t a r. (ForallF c t) => (c (t a) => r) -> r
whichever r = r \\ (instF :: ForallF c t :- c (t a))

-- | Allows explicit specification of constraint implication
class Implies1 c d where
  implies1 :: c a :- d a

