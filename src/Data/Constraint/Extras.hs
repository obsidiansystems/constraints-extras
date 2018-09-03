{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Constraint.Extras where

import Data.Constraint
import Data.Constraint.Forall

-- | Provides proof of the existence of a constraint on a GADT
class ArgDict f where
  type ConstraintsFor f (c :: k -> Constraint) :: Constraint
  type ConstraintsFor' f (c :: k -> Constraint) (g :: k' -> k) :: Constraint
  argDict :: ConstraintsFor f c => f a -> Dict (c a)
  argDict' :: ConstraintsFor' f c g => f a -> Dict (c (g a))

type Has (c :: k -> Constraint) f = (ArgDict f, ConstraintsFor f c)
type Has' (c :: k -> Constraint) f (g :: k' -> k) = (ArgDict f, ConstraintsFor' f c g)

has :: forall c f a r. (Has c f) => f a -> (c a => r) -> r
has k r | (Dict :: Dict (c a)) <- argDict k = r

has' :: forall c g f a r. (Has' c f g) => f a -> (c (g a) => r) -> r
has' k r | (Dict :: Dict (c (g a))) <- argDict' k = r

whichever :: forall c t a r. (ForallF c t) => (c (t a) => r) -> r
whichever r = r \\ (instF :: ForallF c t :- c (t a))

-- | Allows explicit specification of constraint implication
class Implies1 c d where
  implies1 :: c a :- d a
