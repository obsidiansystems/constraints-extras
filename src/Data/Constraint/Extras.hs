{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Constraint.Extras where

import Data.Constraint
import Data.Constraint.Compose
import Data.Constraint.Flip
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

class Has c f where
  has :: forall a r. f a -> (c a => r) -> r
  has x r | Dict <- argDict @c x = r
  argDict :: forall a. f a -> Dict (c a)
  argDict x = has @c x Dict

type Has' (c :: k -> Constraint) f (g :: k' -> k) = Has (ComposeC c g) f

argDict' :: forall c g f a. (Has' c f g) => f a -> Dict (c (g a))
argDict' x = has @(ComposeC c g) x Dict

argDictV :: forall f c g v. (HasV c f g) => f v -> Dict (c (v g))
argDictV x = has @(FlipC (ComposeC c) g) x Dict

type HasV c f g = Has (FlipC (ComposeC c) g) f

has' :: forall c g f a r. (Has' c f g) => f a -> (c (g a) => r) -> r
has' k r = has @(ComposeC c g) k r

hasV :: forall c g f v r. (HasV c f g) => f v -> (c (v g) => r) -> r
hasV k r = has @(FlipC (ComposeC c) g) k r

whichever :: forall c t a r. (ForallF c t) => (c (t a) => r) -> r
whichever r = r \\ (instF :: ForallF c t :- c (t a))

-- | Allows explicit specification of constraint implication
class Implies1 c d where
  implies1 :: c a :- d a

