{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Constraint.Extras.TH where

import Data.Constraint.Extras
import Control.Monad
import Data.Constraint
import Data.Semigroup
import Language.Haskell.TH

deriveArgDict :: Name -> Q [Dec]
deriveArgDict n = do
  ts <- gadtResults n
  c <- newName "c"
  let xs = map (AppT (VarT c)) ts
      l = length xs
      constraints = foldl AppT (TupleT l) xs
  [d| instance ArgDict $(pure $ ConT n) where
        type ConstraintsFor $(varT c) $(pure $ ConT n) = $(pure constraints)
        argDict = $(LamCaseE <$> matches)
    |]
  where
    matches :: Q [Match]
    matches = do
      x <- newName "x"
      reify n >>= \case
        TyConI (DataD _ _ _ _ cons _) -> pure $ concat $ flip map cons $ \case
          GadtC [name] _ (AppT (ConT _) (VarT _)) ->
            [Match (ConP name [VarP x]) (NormalB $ AppE (VarE 'argDict) (VarE x)) []]
          GadtC [name] _ _ ->
            [Match (RecP name []) (NormalB $ ConE 'Dict) []]
          ForallC _ _ (GadtC [name] _ (AppT (ConT _) (VarT _))) ->
            [Match (ConP name [VarP x]) (NormalB $ AppE (VarE 'argDict) (VarE x)) []]
          ForallC _ _ (GadtC [name] _ _) ->
            [Match (RecP name []) (NormalB $ ConE 'Dict) []]
          NormalC name [(_, AppT (ConT _) (VarT _))] ->
            [Match (ConP name [VarP x]) (NormalB $ AppE (VarE 'argDict) (VarE x)) []]
          a -> error $ "deriveArgDict matches: Unmatched 'Dec': " <> show a
        a -> error $ "deriveArgDict matches: Unmatched 'Info': " <> show a

gadtResults :: Name -> Q [Type]
gadtResults n = reify n >>= \case
  TyConI (DataD _ _ _ _ cons _) -> fmap concat $ forM cons $ \case
    GadtC _ _ (AppT (ConT _) (VarT _)) -> return []
    GadtC _ _ (AppT _ (AppT (ConT _) (VarT _))) -> return []
    GadtC _ _ (AppT _ typ) -> return [typ]
    ForallC _ _ (GadtC _ _ (AppT (ConT _) (VarT _))) -> return []
    ForallC _ _ (GadtC _ _ (AppT _ (AppT (ConT _) (VarT _)))) -> return []
    ForallC _ _ (GadtC _ _ (AppT _ typ)) -> return [typ]
    _ -> return []
  a -> error $ "gadtResults: Unmatched 'Info': " <> show a
