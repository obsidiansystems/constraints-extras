{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Constraint.Extras.TH (deriveArgDict, deriveArgDictV, gadtIndices) where

import Data.Constraint.Extras
import Control.Monad
import Data.Constraint
import Data.Maybe
import Language.Haskell.TH

import Data.Either

deriveArgDict :: Name -> Q [Dec]
deriveArgDict n = do
  ts <- gadtIndices n
  c <- newName "c"
  g <- newName "g"
  let xs = flip map ts $ \case
        Left t -> AppT (AppT (ConT ''ConstraintsFor) t) (VarT c)
        Right t -> (AppT (VarT c) t)
      xs' = flip map ts $ \case
        Left t -> AppT (AppT (AppT (ConT ''ConstraintsFor') t) (VarT c)) (VarT g)
        Right t -> AppT (VarT c) (AppT (VarT g) t)
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
  let xs = flip map vs $ \case
        Left t -> AppT (AppT (AppT (ConT ''ConstraintsForV) t) (VarT c)) (VarT g)
        Right v -> AppT (VarT c) $ AppT v (VarT g)
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
    TyConI (DataD _ _ _ _ cons _) -> fmap concat $ forM cons $ \case
      GadtC [name] _ (AppT (ConT _) (VarT _)) -> return $
        [Match (ConP name [VarP x]) (NormalB $ AppE (VarE argDictName) (VarE x)) []]
      GadtC [name] _ _ -> return $
        [Match (RecP name []) (NormalB $ ConE 'Dict) []]
      ForallC _ _ (GadtC [name] bts (AppT (ConT _) (VarT b))) -> do
        ps <- forM bts $ \case
          (_, AppT (ConT a) (VarT b')) | b == b' -> do
            hasArgDictInstance <- not . null <$> reifyInstances ''ArgDict [(ConT a)]
            return $ if hasArgDictInstance
              then Just x
              else Nothing
          _ -> return Nothing
        return $ case catMaybes ps of
          [] -> [Match (RecP name []) (NormalB $ ConE 'Dict) []]
          (v:_) ->
            let patf = \v' rest done -> if done
                  then WildP : rest done
                  else case v' of
                    Nothing -> WildP : rest done
                    Just _ -> VarP v : rest True
                pat = foldr patf (const []) ps False
            in [Match (ConP name pat) (NormalB $ AppE (VarE argDictName) (VarE v)) []]
      ForallC _ _ (GadtC [name] _ _) -> return $
        [Match (RecP name []) (NormalB $ ConE 'Dict) []]
      a -> error $ "deriveArgDict matches: Unmatched 'Dec': " <> show a
    a -> error $ "deriveArgDict matches: Unmatched 'Info': " <> show a

gadtIndices :: Name -> Q [Either Type Type]
gadtIndices n = do
  reify n >>= \case
    TyConI (DataD _ _ _ _ cons _) -> fmap concat $ forM cons $ \x -> case x of
      GadtC _ _ (AppT (ConT _) (VarT _)) -> return []
      GadtC _ _ (AppT _ typ) -> return [Right typ]
      ForallC _ _ (GadtC _ bts (AppT (ConT _) (VarT _))) -> fmap concat $ forM bts $ \case
        (_, AppT (ConT a) (VarT _)) -> do
          hasArgDictInstance <- fmap (not . null) $ reifyInstances ''ArgDict [(ConT a)]
          return $ if hasArgDictInstance then [Left (ConT a)] else []
        _ -> return []
      ForallC _ _ (GadtC _ _ (AppT _ typ)) -> return [Right typ]
      _ -> return []
    a -> error $ "gadtResults: Unmatched 'Info': " <> show a
