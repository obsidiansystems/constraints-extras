# constraints-extras

## Example usage:

NB: This example can be built with `-pgmL markdown-unlit`.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

import Control.Monad

import Data.GADT.Generics
import Data.GADT.Generics.TH

data A :: * -> * where
  A_a :: A Int
  A_b :: Int -> A ()

data B :: * -> * where
  B_a :: A a -> A a -> B a
  B_x :: Int -> B Int

data V :: (* -> *) -> * where
  V_a :: A Int -> V A

deriveGGeneric ''A
deriveGGeneric ''B
deriveGGeneric ''V

main :: IO ()
main = do
  guard $ toIndex A_a == SExprCursor_Zero (SExprCursorNode_Atom @Int)
  guard $ toIndex (A_b 0) == SExprCursor_Succ (SExprCursor_Zero (SExprCursorNode_Atom @()))
  let
    f :: A a -> Bool
    f a = toIndex (B_a a a) == SExprCursor_Zero (SExprCursorNode_List (toIndex a))
  guard $ f A_a
  guard $ f $ A_b 1
  guard $ toIndex (V_a A_a) == SExprCursor_Zero (SExprCursorNode_Atom @A)
```
