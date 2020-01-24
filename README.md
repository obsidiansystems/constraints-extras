constraints-extras [![travis-ci](https://api.travis-ci.org/obsidiansystems/constraints-extras.svg?branch=develop)](https://travis-ci.org/obsidiansystems/constraints-extras)
==================

Example usage:
--------------

```haskell
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
```

A "simple" GADT.

```haskell
> data A :: * -> * where
>   A_a :: A Int
>   A_b :: Int -> A ()
>
> deriveArgDict ''A
```

A GADT which uses `A`.


```haskell
> data B :: * -> * where
>   B_a :: A a -> A a -> B a
>   B_x :: Int -> B Int
>
> deriveArgDict ''B
```

A GADT which has a non-`Type` parameter.


```haskell
> data V :: (* -> *) -> * where
>   V_a :: A Int -> V A
>
> deriveArgDict ''V
```

Now let's actually use them


```haskell
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
```

We can hand-write an instance for there being non-finite indices.


```haskell
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
```

We have the instances we want:


```haskell
> abstractConstraintWitnesses :: Has c SimpleExpr => Dict (c Int, c Bool, c [Int], c [Bool], c [[Int]], c [[Bool]])
> abstractConstraintWitnesses = Dict
```


```haskell
> concreteClassSmokeTest :: Dict (Has Show SimpleExpr)
> concreteClassSmokeTest = Dict
```

Even in when we have no "slack" (instances beyond what `Has` requires):


```haskell
> class Minimal a
> instance Minimal Int
> instance Minimal Bool
> instance Minimal a => Minimal [a]
```

```haskell
> minimalWitness :: Dict (Has Minimal SimpleExpr)
> minimalWitness = Dict

The funny "Leibnitz-style" `forall c'` is so fancier things than Minimal
(which also might not be satisfied for other args) also work:


```haskell
> class MinimalPing a
> class MinimalPong a
> instance MinimalPing Int
> instance MinimalPong Int
> instance MinimalPing Bool
> instance MinimalPong Bool
> instance MinimalPing a => MinimalPong [a]
> instance MinimalPong a => MinimalPing [a]

> minimalPingPongWitness :: Dict (Has MinimalPing SimpleExpr, Has MinimalPong SimpleExpr)
> minimalPingPongWitness = Dict
```

We can also hand-write instances which take advantage of constructor's dictionaries

```haskell
> data WithOrd a where
>   WithOrd :: Ord a => WithOrd a
>
> -- class to avoid impredicativity
> class (forall a. Ord a => c a) => ConstraintsForWithOrd c
> instance (forall a. Ord a => c a) => ConstraintsForWithOrd c
>
> instance ArgDict WithOrd where
>   type ConstraintsFor WithOrd c = ConstraintsForWithOrd c
>   argDictAll WithOrd = Dict
```

Now we can use the constructor dictionary to discharge constraints.
We can get out `Ord a`:


```haskell
> useThisOrd :: WithOrd a -> a -> a -> Ordering
> useThisOrd wo x y = has @Ord wo $ x `compare` y
```

and things derivable from it:


```haskell
> useThisOrdImplication :: WithOrd a -> [a] -> [a] -> (Bool, Bool)
> useThisOrdImplication wo x y =
>  ( has @Ord wo $ x == y      -- implicit way
>  , has' @Eq @[] wo $ x == y  -- explicit way
>  )
```

Oh, and let's make this README build


```haskell
> main :: IO ()
> main = return ()
```
