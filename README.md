# constraints-extras

## Example usage:

NB: This example can be built with `-pgmL markdown-unlit`.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

import Data.Aeson
import Data.Constraint.Forall
import Data.Constraint.Extras
import Data.Constraint.Extras.TH

data A :: * -> * where
  A_a :: A Int
  A_b :: Int -> A ()

data B :: * -> * where
  B_a :: A a -> A a -> B a
  B_x :: Int -> B Int

data V :: (* -> *) -> * where
  V_a :: A Int -> V A

deriveArgDict ''A
deriveArgDict ''B
deriveArgDictV ''V

data DSum k f = forall a. DSum (k a) (f a)

-- Derive a ToJSON instance for our 'DSum'
instance forall k f.
  ( Has' ToJSON k f -- Given a value of type (k a), we can obtain an instance (ToJSON (f a))
  , ForallF ToJSON k -- For any (a), we have an instance (ToJSON (k a))
  ) => ToJSON (DSum k f) where
  toJSON (DSum (k :: k a) f) = toJSON
    ( whichever @ToJSON @k @a $ toJSON k -- Use the (ForallF ToJSON k) constraint to obtain the (ToJSON (k a)) instance
    , has' @ToJSON @f k $ toJSON f -- Use the (Has' ToJSON k f) constraint to obtain the (ToJSON (f a)) instance
    )

data Some k = forall a. Some (k a)

-- Derive a FromJSON instance for our 'DSum'
instance (FromJSON (Some f), Has' FromJSON f g) => FromJSON (DSum f g) where
  parseJSON x = do
    (jf, jg) <- parseJSON x
    Some (f :: f a) <- parseJSON jf
    g <- has' @FromJSON @g f (parseJSON jg)
    return $ DSum f g

main :: IO ()
main = return ()
```
