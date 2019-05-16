{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Constraint.Extras.TH (deriveArgDict, deriveArgDictV, gadtIndices) where

import Data.Constraint.Extras
import Data.Constraint
import Data.Either
import Data.Maybe
import Control.Monad
import Language.Haskell.TH

deriveArgDict :: Name -> Q [Dec]
deriveArgDict n = do
  c <- newName "c"
  ts <- gadtIndices c n
  let xs = flip map ts $ \case
        Left t -> AppT (AppT (ConT ''ConstraintsFor) t) (VarT c)
        Right t -> (AppT (VarT c) t)
      l = length xs
      constraints = foldl AppT (TupleT l) xs
  arity <- tyConArity n
  tyVars <- replicateM (arity - 1) (newName "a")
  let n' = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
  [d| instance ArgDict $(varT c) $(pure n') where
        type ConstraintsFor  $(pure n') $(varT c) = $(pure constraints)
        argDict = $(LamCaseE <$> matches c n 'argDict)
    |]

{-# DEPRECATED deriveArgDictV "Just use 'deriveArgDict'" #-}
deriveArgDictV = deriveArgDict

matches :: Name -> Name -> Name -> Q [Match]
matches c n argDictName = do
  x <- newName "x"
  reify n >>= \case
    TyConI (DataD _ _ _ _ cons _) -> fmap concat $ forM cons $ \case
      GadtC [name] _ _ -> return $
        [Match (RecP name []) (NormalB $ ConE 'Dict) []]
      ForallC _ _ (GadtC [name] bts (AppT _ (VarT b))) -> do
        ps <- forM bts $ \case
          (_, AppT t (VarT b')) | b == b' -> do
            hasArgDictInstance <- not . null <$> reifyInstances ''ArgDict [VarT c, t]
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
      a -> error $ "deriveArgDict matches: Unmatched 'Dec': " ++ show a
    a -> error $ "deriveArgDict matches: Unmatched 'Info': " ++ show a

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
   _ -> error $ "tyConArity: Supplied name reified to something other than a data declaration: " ++ show n

gadtIndices :: Name -> Name -> Q [Either Type Type]
gadtIndices c n = reify n >>= \case
  TyConI (DataD _ _ _ _ cons _) -> fmap concat $ forM cons $ \case
    GadtC _ _ (AppT _ typ) -> return [Right typ]
    ForallC _ _ (GadtC _ bts (AppT _ (VarT _))) -> fmap concat $ forM bts $ \case
      (_, AppT t (VarT _)) -> do
        hasArgDictInstance <- fmap (not . null) $ reifyInstances ''ArgDict [VarT c, t]
        return $ if hasArgDictInstance then [Left t] else []
      _ -> return []
    ForallC _ _ (GadtC _ _ (AppT _ typ)) -> return [Right typ]
    _ -> return []
  a -> error $ "gadtIndices: Unmatched 'Info': " ++ show a
