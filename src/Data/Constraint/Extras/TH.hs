{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Constraint.Extras.TH (deriveArgDict, deriveArgDictV, gadtIndices) where

import Data.Constraint.Extras
import Control.Monad
import Data.Constraint
import Data.Semigroup
import Language.Haskell.TH

deriveArgDict :: Name -> Q [Dec]
deriveArgDict n = do
  ts <- gadtIndices n
  c <- newName "c"
  g <- newName "g"
  let xs = map (AppT (VarT c)) ts
      xs' = map (AppT (VarT c) . AppT (VarT g)) ts
      l = length xs
      constraints = foldl AppT (TupleT l) xs
      constraints' = foldl AppT (TupleT l) xs'
  {-
  runIO $ putStrLn "Constraints:"
  runIO . putStrLn . pprint $ constraints'
  -}
  [d| instance ArgDict $(pure $ ConT n) where
        type ConstraintsFor  $(conT n) $(varT c) = $(pure constraints)
        type ConstraintsFor' $(conT n) $(varT c) $(varT g) = $(pure constraints')
        argDict = $(LamCaseE <$> matches n 'argDict)
        argDict' = $(LamCaseE <$> matches n 'argDict')
    |]

deriveArgDictV :: Name -> Q [Dec]
deriveArgDictV n = do
  vs <- gadtIndices n
  c <- newName "c"
  g <- newName "g"
  let xs = map (\v -> AppT (VarT c) $ AppT v (VarT g)) vs
      l = length xs
      constraints = foldl AppT (TupleT l) xs
  {-
  runIO $ putStrLn "Constraints:"
  runIO . putStrLn . pprint $ constraints'
  -}
  ds <- deriveArgDict n
  d <- [d| instance ArgDictV $(pure $ ConT n) where
             type ConstraintsForV $(conT n) $(varT c) $(varT g) = $(pure constraints)
             argDictV = $(LamCaseE <$> matches n 'argDictV)
       |]
  return (d ++ ds)

matches :: Name -> Name -> Q [Match]
matches n argDictName = do
  x <- newName "x"
  reify n >>= \case
    TyConI (DataD _ _ _ _ cons _) -> pure $ concat $ flip map cons $ \case
      GadtC [name] _ (AppT (ConT _) (VarT _)) ->
        [Match (ConP name [VarP x]) (NormalB $ AppE (VarE argDictName) (VarE x)) []]
      GadtC [name] _ _ ->
        [Match (RecP name []) (NormalB $ ConE 'Dict) []]
      ForallC _ _ (GadtC [name] _ (AppT (ConT _) (VarT _))) ->
        [Match (ConP name [VarP x]) (NormalB $ AppE (VarE argDictName) (VarE x)) []]
      ForallC _ _ (GadtC [name] _ _) ->
        [Match (RecP name []) (NormalB $ ConE 'Dict) []]
      NormalC name [(_, AppT (ConT _) (VarT _))] ->
        [Match (ConP name [VarP x]) (NormalB $ AppE (VarE argDictName) (VarE x)) []]
      a -> error $ "deriveArgDict matches: Unmatched 'Dec': " <> show a
    a -> error $ "deriveArgDict matches: Unmatched 'Info': " <> show a

gadtIndices :: Name -> Q [Type]
gadtIndices n = do
  reify n >>= \case
    TyConI (DataD _ _ _ _ cons _) -> fmap concat $ forM cons $ \case
      GadtC _ _ (AppT (ConT _) (VarT _)) -> return []
      GadtC _ _ (AppT _ typ) -> return [typ]
      ForallC _ _ (GadtC _ _ (AppT (ConT _) (VarT _))) -> return []
      ForallC _ _ (GadtC _ _ (AppT _ typ)) -> return [typ]
      _ -> return []
    a -> error $ "gadtResults: Unmatched 'Info': " <> show a
