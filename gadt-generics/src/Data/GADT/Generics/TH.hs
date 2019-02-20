{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.GADT.Generics.TH
  ( deriveGGeneric
  , gadtIndices
  ) where

import Data.GADT.Generics
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid (Endo (..))
import Language.Haskell.TH

deriveGGeneric :: Name -> Q [Dec]
deriveGGeneric n = do
  ts <- gadtIndices n
  let xs :: [Type]
      xs = flip map ts $ \case
        Left t -> AppT (ConT 'SExpr_List) $ AppT (ConT ''Indices) t
        Right t -> AppT (ConT 'SExpr_Atom) t
      types = foldr (AppT . AppT (ConT '(:))) (ConT '[]) xs
  arity <- tyConArity n
  tyVars <- replicateM (arity - 1) (newName "a")
  let n' = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
  [d| instance GGeneric $(pure n') where
        type Indices $(pure n') = $(pure types)
        toIndex = $(LamCaseE <$> matches n 'toIndex)
    |]

matches :: Name -> Name -> Q [Match]
matches n toIndexName = do
  x <- newName "x"
  reify n >>= \case
    TyConI (DataD _ _ _ _ constrs _) -> fmap concat $ ifor constrs $ \idx -> \case
      GadtC [name] _ _ -> return $
        [Match (RecP name []) (NormalB $ succApp idx atom) []]
        where
      ForallC _ _ (GadtC [name] bts (AppT _ (VarT b))) -> do
        ps <- forM bts $ \case
          (_, AppT t (VarT b')) | b == b' -> do
            hasGGenericInstance <- not . null <$> reifyInstances ''GGeneric [t]
            return $ if hasGGenericInstance
              then Just x
              else Nothing
          _ -> return Nothing
        return $ case catMaybes ps of
          [] -> [Match (RecP name []) (NormalB $ succApp idx atom) []]
          (v:_) ->
            let patf = \v' rest done -> if done
                  then WildP : rest done
                  else case v' of
                    Nothing -> WildP : rest done
                    Just _ -> VarP v : rest True
                pat = foldr patf (const []) ps False
            in pure $ Match
                 (ConP name pat)
                 (NormalB $ succApp idx $ AppE list $ AppE (VarE toIndexName) $ VarE v)
                 []
      ForallC _ _ (GadtC [name] _ _) -> return $
        [Match (RecP name []) (NormalB $ succApp idx atom) []]
      a -> error $ "deriveGGeneric matches: Unmatched 'Dec': " <> show a
    a -> error $ "deriveGGeneric matches: Unmatched 'Info': " <> show a
    where
      succApp idx = appEndo (fold $ succs idx) . AppE (ConE 'SExprCursor_Zero)
      succs idx = replicate idx $ Endo $ AppE $ ConE 'SExprCursor_Succ
      atom = ConE 'SExprCursorNode_Atom
      list = ConE 'SExprCursorNode_List

kindArity :: Kind -> Int
kindArity = \case
  ForallT _ _ t -> kindArity t
  AppT (AppT ArrowT _) t -> 1 + kindArity t
  SigT t _ -> kindArity t
  ParensT t -> kindArity t
  _ -> 0

tyConArity :: Name -> Q Int
tyConArity n = reify n >>= return . \case
   TyConI (DataD _ _ ts mk _ _) -> fromMaybe 0 (fmap kindArity mk) + length ts
   _ -> error $ "tyConArity: Supplied name reified to something other than a data declaration: " <> show n

gadtIndices :: Name -> Q [Either Type Type]
gadtIndices n = reify n >>= \case
  TyConI (DataD _ _ _ _ constrs _) -> forM constrs $ \case
    GadtC _ _ (AppT _ typ) -> return $ Right typ
    ForallC _ _ (GadtC _ bts (AppT _ (VarT _))) -> fmap (head . catMaybes) $ forM bts $ \case
      (_, AppT t (VarT _)) -> do
        hasGGenericInstance <- fmap (not . null) $ reifyInstances ''GGeneric [t]
        pure $ do
          guard $ hasGGenericInstance
          pure $ Left t
      _ -> return Nothing
    ForallC _ _ (GadtC _ _ (AppT _ typ)) -> return $ Right typ
    a -> error $ "gadtResults: Unmatched 'Dec': " <> show a
  a -> error $ "gadtResults: Unmatched 'Info': " <> show a
