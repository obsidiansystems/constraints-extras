{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Constraint.Extras.TH
  ( deriveArgDict
  , deriveArgDict'
  , deriveArgDictV
  , deriveArgDictV'
  , gadtIndices
  ) where

import Data.Constraint.Extras
import Control.Monad
import Data.Constraint
import Data.Semigroup
import Language.Haskell.TH

deriveArgDict :: Name -> Q [Dec]
deriveArgDict n = do
  c <- countTypeNameArgs n
  if c >= 1
    then deriveArgDict' (c-1) n
    else error $ "deriveArgDict: type `" <> show n <> "' doesn't appear to have a parameter."

deriveArgDict' :: Word -> Name -> Q [Dec]
deriveArgDict' numArgs n = do
  ts <- gadtIndices n
  c <- newName "c"
  g <- newName "g"
  let xs = map (AppT (VarT c)) ts
      xs' = map (AppT (VarT c) . AppT (VarT g)) ts
      l = length xs
      constraints = foldl AppT (TupleT l) xs
      constraints' = foldl AppT (TupleT l) xs'
  instHead <- makeInstanceHead n numArgs
  {-
  runIO $ putStrLn "Constraints:"
  runIO . putStrLn . pprint $ constraints'
  -}
  [d| instance ArgDict $(pure instHead) where
        type ConstraintsFor  $(pure instHead) $(varT c) = $(pure constraints)
        type ConstraintsFor' $(pure instHead) $(varT c) $(varT g) = $(pure constraints')
        argDict = $(LamCaseE <$> matches n 'argDict)
        argDict' = $(LamCaseE <$> matches n 'argDict')
    |]

deriveArgDictV :: Name -> Q [Dec]
deriveArgDictV n = do
  c <- countTypeNameArgs n
  if c >= 1
    then deriveArgDictV' (c-1) n
    else error $ "deriveArgDictV: type `" <> show n <> "' doesn't appear to have a parameter."


deriveArgDictV' :: Word -> Name -> Q [Dec]
deriveArgDictV' numArgs n = do
  vs <- gadtIndices n
  c <- newName "c"
  g <- newName "g"
  let xs = map (\v -> AppT (VarT c) $ AppT v (VarT g)) vs
      l = length xs
      constraints = foldl AppT (TupleT l) xs
  instHead <- makeInstanceHead n numArgs
  {-
  runIO $ putStrLn "Constraints:"
  runIO . putStrLn . pprint $ constraints'
  -}
  ds <- deriveArgDict n
  d <- [d| instance ArgDictV $(pure instHead) where
             type ConstraintsForV $(pure instHead) $(varT c) $(varT g) = $(pure constraints)
             argDictV = $(LamCaseE <$> matches n 'argDictV)
       |]
  return (d ++ ds)

makeInstanceHead :: Name -> Word -> TypeQ
makeInstanceHead n numArgs = go numArgs
  where go 0 = conT n
        go n = do
          arg <- newName $ "arg" ++ show n
          AppT <$> go (n - 1) <*> pure (VarT arg)

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
    a -> error $ "gadtIndices: Unmatched 'Info': " <> show a

-- Haruspication to determine the number of arguments of a named type.
countTypeNameArgs :: Name -> Q Word
countTypeNameArgs n = do
  reify n >>= \case
    TyConI (DataD _ _ explicitArgs mKind _ _) ->
      return $ fromIntegral (length explicitArgs) + maybe 0 typeArity mKind
    a -> error $ "countArgs: Unmatched 'Info': " <> show a

-- Determine the arity of a function type, or zero otherwise.
typeArity :: Type -> Word
typeArity t = case t of
  ForallT _ _ t -> typeArity t
  AppT (AppT ArrowT _) t -> 1 + typeArity t
  _ -> 0