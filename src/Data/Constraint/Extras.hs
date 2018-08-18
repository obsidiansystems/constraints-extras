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
  type ConstraintsFor (c :: k -> Constraint) f :: Constraint
  argDict :: ConstraintsFor c f => f a -> Dict (c a)

type Has c f = (ArgDict f, ConstraintsFor c f)

has :: forall c f a r. (Has c f) => f a -> (c a => r) -> r
has k r | (Dict :: Dict (c a)) <- argDict k = r

whichever :: forall c t a r. (ForallF c t) => (c (t a) => r) -> r
whichever r = r \\ (instF :: ForallF c t :- c (t a))

-- | Allows explicit specification of constraint implication
class Implies1 c d where
  implies1 :: c a :- d a
