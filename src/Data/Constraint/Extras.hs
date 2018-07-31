{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.Extras where

import Data.Constraint

-- | Provides proof of the existence of a constraint on a GADT
class ArgDict f where
  type ConstraintsFor (c :: * -> Constraint) f :: Constraint
  argDict :: ConstraintsFor c f => f a -> Dict (c a)

type Has c f = (ArgDict f, ConstraintsFor c f)

-- | Allows explicit specification of constraint implication
class Implies1 c d where
  implies1 :: c a :- d a
