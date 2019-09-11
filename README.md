constraints-extras [![travis-ci](https://api.travis-ci.org/obsidiansystems/constraints-extras.svg?branch=develop)](https://travis-ci.org/obsidiansystems/constraints-extras)
==================

Example usage:
--------------

> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE QuantifiedConstraints #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeApplications  #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE UndecidableSuperClasses #-}
>
> {-# OPTIONS_GHC -Wno-unused-top-binds #-}
>
> import Data.Aeson
> import Data.Constraint
> import Data.Constraint.Forall
> import Data.Constraint.Extras
> import Data.Constraint.Extras.TH
>

A "simple" GADT.

> data A :: * -> * where
>   A_a :: A Int
>   A_b :: Int -> A ()
>
> deriveArgDict ''A

A GADT which uses `A`.

> data B :: * -> * where
>   B_a :: A a -> A a -> B a
>   B_x :: Int -> B Int
>
> deriveArgDict ''B

A GADT which has a non-`Type` parameter.

> data V :: (* -> *) -> * where
>   V_a :: A Int -> V A
>
> deriveArgDict ''V

Now let's actually use them

> data DSum k f = forall a. DSum (k a) (f a)
>
> -- Derive a ToJSON instance for our 'DSum'
> instance forall k f.
>   ( Has' ToJSON k f -- Given a value of type (k a), we can obtain an instance (ToJSON (f a))
>   , ForallF ToJSON k -- For any (a), we have an instance (ToJSON (k a))
>   ) => ToJSON (DSum k f) where
>   toJSON (DSum (k :: k a) f) = toJSON
>     ( whichever @ToJSON @k @a $ toJSON k -- Use the (ForallF ToJSON k) constraint to obtain the (ToJSON (k a)) instance
>     , has' @ToJSON @f k $ toJSON f -- Use the (Has' ToJSON k f) constraint to obtain the (ToJSON (f a)) instance
>     )
>
> data Some k = forall a. Some (k a)
>
> -- Derive a FromJSON instance for our 'DSum'
> instance (FromJSON (Some f), Has' FromJSON f g) => FromJSON (DSum f g) where
>   parseJSON x = do
>     (jf, jg) <- parseJSON x
>     Some (f :: f a) <- parseJSON jf
>     g <- has' @FromJSON @g f (parseJSON jg)
>     return $ DSum f g

We can hand-write an instance for there being non-finite indices.

> data SimpleExpr :: * -> * where
>   SimpleExpr_BoolLit :: Bool -> SimpleExpr Bool
>   SimpleExpr_IntLit :: Int -> SimpleExpr Int
>   -- crude non-empty list
>   SimpleExpr_ListLit :: SimpleExpr a -> [SimpleExpr a] -> SimpleExpr [a]
>
> class
>   ( c Int
>   , c Bool
>   , (forall a. (forall c'. ConstraintsFor SimpleExpr c' => c' a) => c [a])
>   ) => ConstraintsForSimpleExpr c
> instance
>   ( c Int
>   , c Bool
>   , (forall a. (forall c'. ConstraintsFor SimpleExpr c' => c' a) => c [a])
>   ) => ConstraintsForSimpleExpr c
>
> instance ArgDict SimpleExpr where
>   type ConstraintsFor SimpleExpr c = ConstraintsForSimpleExpr c
>   argDictAll = go
>     where
>       go :: forall c a. ConstraintsFor SimpleExpr c => SimpleExpr a -> Dict (c a)
>       go = \case
>         SimpleExpr_BoolLit _ -> Dict
>         SimpleExpr_IntLit _ -> Dict
>         SimpleExpr_ListLit h _ -> hasAll h Dict

We have the instances we want:

> abstractConstraintWitnesses :: Has c SimpleExpr => Dict (c Int, c Bool, c [Int], c [Bool], c [[Int]], c [[Bool]])
> abstractConstraintWitnesses = Dict

> concreteClassSmokeTest :: Dict (Has Show SimpleExpr)
> concreteClassSmokeTest = Dict

Even in when we have no "slack" (instances beyond what `Has` requires):

> class Minimal a
> instance Minimal Int
> instance Minimal Bool
> instance Minimal a => Minimal [a]

> minimalWitness :: Dict (Has Minimal SimpleExpr)
> minimalWitness = Dict

Oh, and let's make this README build

> main :: IO ()
> main = return ()
