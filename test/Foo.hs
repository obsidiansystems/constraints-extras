{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
module Foo where

import Data.Kind

import Data.Constraint.Extras.TH

data Foo :: Type -> Type -> Type where
  Foo_Foo :: Foo Int Int
  Foo_Bar :: Foo Int Bool

deriveArgDict' 1 ''Foo
