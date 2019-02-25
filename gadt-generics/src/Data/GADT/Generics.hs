{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.GADT.Generics where

import Control.Lens (Iso', iso)

-- | An S-Expression, as in Lisp.
--
-- This is used to represent the structure of nested GADTs. Each
-- constructor/variant becomes an element of a list. If the constructor has a
-- GADT child that decides the type parameter, it's s-expression representation
-- is nested as a sublist, otherwise the element is an atom containing the type
-- of the phantom parameter.
data SExpr t
  = SExpr_Atom t
  | SExpr_List [SExpr t]
  deriving (Eq, Ord, Show)

-- | A cursor/index into a list of s-expressions. This is mutually recursive
-- with 'SExprCursorNode'. We use '[SExpr k]' rather than 'SExpr k' to show that
-- the top level is always a list an never an atom.
data SExprCursor :: [SExpr k] -> k -> * where
  SExprCursor_Zero
    :: SExprCursorNode node ty
    -> SExprCursor (node ': nodes) ty
  SExprCursor_Succ
    :: SExprCursor nodes ty
    -> SExprCursor (node ': nodes) ty

deriving instance Eq (SExprCursor sexpr k)
deriving instance Ord (SExprCursor sexpr k)
deriving instance Show (SExprCursor sexpr k)

-- | A cursor/index into an s-expression. Basically, a list of natural numbers
-- that index successive sublists. The s-expression to be index is promoted and
-- taken as a parameter; this ensures all numbers are in bounds for the list
-- they index, and the number indices matches the depth of nested lists at that
-- point.
data SExprCursorNode :: SExpr k -> k -> * where
  SExprCursorNode_Atom
    :: SExprCursorNode ('SExpr_Atom ty) ty
  SExprCursorNode_List
    :: SExprCursor nodes ty
    -> SExprCursorNode ('SExpr_List nodes) ty

deriving instance Eq (SExprCursorNode sexpr k)
deriving instance Ord (SExprCursorNode sexpr k)
deriving instance Show (SExprCursorNode sexpr k)

data CursorWithExtra :: ((*, phantomK) -> *) -> phantomK -> * where
  CursorWithExtra :: k '(normal, phantomT) -> normal -> CursorWithExtra k phantomT

-- | This class reifies the structure of a GADT 'f' whose variants can be
-- finitely enumerated.
class GGeneric (f :: phantomK -> *) where
  -- | 'Indices' represents the GADT type as an s-expression containing an atom for
  -- every variant with that variant's index. Subexpressions represent nested
  -- GADTs, to preserve embedding.
  type Indices f :: [SExpr (*, phantomK)]

  -- | Takes a term of GADT 'f a' and transforms it into a cursor into the generic
  -- description of 'f'.
  toIndex :: f a -> CursorWithExtra (SExprCursor (Indices f)) a

  -- | Takes a cursor into the generic representation of 'f' and transforms it
  -- into a term of GADT 'f a'.
  fromIndex :: CursorWithExtra (SExprCursor (Indices f)) a -> f a

  -- | Combines 'toIndex' and 'fromIndex' into an isomorphism.
  isoIndex :: Iso' (f a) (CursorWithExtra (SExprCursor (Indices f)) a)
  isoIndex = iso toIndex fromIndex
