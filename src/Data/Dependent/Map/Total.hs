{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Dependent.Map.Total where

import Control.Lens
import Data.Constraint.Extras

newtype TotalDMap k f = TotalDMap { unTotalDMap :: TotalDMapInner f (Indices k) }

data TotalDMapInner :: (k -> *) -> [SExpr k] -> * where
  TotalDMapInner_Nil
    :: TotalDMapInner f '[]
  TotalDMapInner_Cons
    :: TotalDMapNode f node
    -> TotalDMapInner f nodes
    -> TotalDMapInner f (node ': nodes)

data TotalDMapNode :: (k -> *) -> SExpr k -> * where
  TotalDMapNode_Atom
    :: f ty
    -> TotalDMapNode f ('SExpr_Atom ty)
  TotalDMapNode_List
    :: TotalDMapInner f children
    -> TotalDMapNode f ('SExpr_List children)

lenses'
  :: forall sexprs g a f
  .  Functor f
  => SExprCursor sexprs a
  -> Optic' (->) f (TotalDMapInner g sexprs) (g a)
lenses' c0 f t0 = go c0 t0
  where
    go
      :: forall sexprs'
      .  SExprCursor sexprs' a
      -> TotalDMapInner g sexprs'
      -> f (TotalDMapInner g sexprs')
    go (SExprCursor_Zero nodeC) (TotalDMapInner_Cons node nodes) =
      (\x -> TotalDMapInner_Cons x nodes) <$> go' nodeC node
    go (SExprCursor_Succ c) (TotalDMapInner_Cons node nodes) =
      TotalDMapInner_Cons node <$> go c nodes

    go'
      :: forall sexpr
      .  SExprCursorNode sexpr a
      -> TotalDMapNode g sexpr
      -> f (TotalDMapNode g sexpr)
    go' SExprCursorNode_Atom (TotalDMapNode_Atom atom) =
      TotalDMapNode_Atom <$> f atom
    go' (SExprCursorNode_List subCursor) (TotalDMapNode_List children') =
      TotalDMapNode_List <$> go subCursor children'

lenses
  :: ArgDict k
  => k a
  -> Lens' (TotalDMap k f) (f a)
lenses c f t = TotalDMap <$> lenses' (toIndex c) f (unTotalDMap t)
