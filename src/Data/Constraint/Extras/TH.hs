{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Constraint.Extras.TH (deriveArgDict, deriveArgDictV, gadtIndices) where

import Control.Monad
import Data.Constraint
import Data.Constraint.Extras
import Data.Maybe
import Language.Haskell.TH

deriveArgDict :: Name -> Q [Dec]
deriveArgDict n = do
  ts <- gadtIndices n
  c <- newName "c"
  let xs = flip map ts $ \case
        Left t -> AppT (AppT (ConT ''ConstraintsFor) t) (VarT c)
        Right t -> (AppT (VarT c) t)
      l = length xs
      constraints = foldl AppT (TupleT l) xs
  arity <- tyConArity n
  tyVars <- replicateM (arity - 1) (newName "a")
  let n' = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
  [d| instance ArgDict $(pure n') where
        type ConstraintsFor  $(pure n') $(varT c) = $(pure constraints)
        argDict = $(LamCaseE <$> matches n 'argDict)
    |]

{-# DEPRECATED deriveArgDictV "Just use 'deriveArgDict'" #-}
deriveArgDictV :: Name -> Q [Dec]
deriveArgDictV = deriveArgDict

matches :: Name -> Name -> Q [Match]
matches n argDictName = do
  x <- newName "x"
  reify n >>= \case
    TyConI (DataD _ _ _ _ constrs _) -> fmap concat $ forM constrs $ \case
      GadtC [name] _ _ -> return $
        [Match (RecP name []) (NormalB $ ConE 'Dict) []]
      ForallC _ _ (GadtC [name] bts (AppT _ (VarT b))) -> do
        ps <- forM bts $ \case
          (_, AppT t (VarT b')) | b == b' -> do
            hasArgDictInstance <- not . null <$> reifyInstances ''ArgDict [t]
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
        hasArgDictInstance <- fmap (not . null) $ reifyInstances ''ArgDict [t]
        pure $ do
          guard $ hasArgDictInstance
          pure $ Left t
      _ -> return Nothing
    ForallC _ _ (GadtC _ _ (AppT _ typ)) -> return $ Right typ
    a -> error $ "gadtResults: Unmatched 'Dec': " <> show a
  a -> error $ "gadtResults: Unmatched 'Info': " <> show a
