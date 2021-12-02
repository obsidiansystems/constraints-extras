{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Constraint.Extras.TH (deriveArgDict, deriveArgDictV, gadtIndices) where

import Data.Constraint
import Data.Constraint.Extras
import Data.Maybe
import Control.Monad
import Language.Haskell.TH

deriveArgDict :: Name -> Q [Dec]
deriveArgDict n = do
  (typeHead, constrs) <- getDeclInfo n
  ts <- gadtIndices constrs
  c <- newName "c"
  let xs = flip map ts $ \case
        Left t -> AppT (AppT (ConT ''ConstraintsFor) t) (VarT c)
        Right t -> (AppT (VarT c) t)
      l = length xs
      constraints = foldl AppT (TupleT l) xs
  [d| instance ArgDict $(pure typeHead) where
        type ConstraintsFor  $(pure typeHead) $(varT c) = $(pure constraints)
        argDictAll = $(LamCaseE <$> matches constrs 'argDictAll)
    |]

{-# DEPRECATED deriveArgDictV "Just use 'deriveArgDict'" #-}
deriveArgDictV :: Name -> Q [Dec]
deriveArgDictV = deriveArgDict

matches :: [Con] -> Name -> Q [Match]
matches constrs argDictName = do
  x <- newName "x"
  fmap concat $ forM constrs $ \case
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
              eta = CaseE (AppE (VarE argDictName) (VarE v))
                [Match (RecP 'Dict []) (NormalB $ ConE 'Dict) []]
          in [Match (conPCompat name pat) (NormalB eta) []]
    ForallC _ _ (GadtC [name] _ _) -> return $
      [Match (RecP name []) (NormalB $ ConE 'Dict) []]
    a -> error $ "deriveArgDict matches: Unmatched 'Dec': " ++ show a

conPCompat :: Name -> [Pat] -> Pat
conPCompat name =
  ConP
    name
#if MIN_VERSION_template_haskell(2, 18, 0)
    []
#endif

kindArity :: Kind -> Int
kindArity = \case
  ForallT _ _ t -> kindArity t
  AppT (AppT ArrowT _) t -> 1 + kindArity t
  SigT t _ -> kindArity t
  ParensT t -> kindArity t
  _ -> 0

getDeclInfo :: Name -> Q (Type, [Con])
getDeclInfo n = reify n >>= \case
  TyConI (DataD _ _ ts mk constrs _) -> do
    let arity = fromMaybe 0 (fmap kindArity mk) + length ts
    tyVars <- replicateM (arity - 1) (newName "a")
    let typeHead = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
    return (typeHead, constrs)
  DataConI _ (ForallT _ _ (AppT typeHead _)) parent -> do
    reify parent >>= \case
      FamilyI _ instances -> do
        let instCons :: InstanceDec -> [Con]
            instCons = \case
              DataInstD _ _ _ _ cons _ -> cons
              NewtypeInstD _ _ _ _ con _ -> [con]
              _ -> error $ "getDeclInfo: Expected a data or newtype family instance"
            conNames :: Con -> [Name]
            conNames = \case
              NormalC other _ -> [other]
              RecC other _ -> [other]
              InfixC _ other _ -> [other]
              ForallC _ _ con -> conNames con
              GadtC others _ _ -> others
              RecGadtC others _ _ -> others
            instHasThisConstructor i = any (== n) $ conNames =<< instCons i
        case filter instHasThisConstructor instances of
          [] -> error $ "getDeclInfo: Couldn't find data family instance for constructor " ++ show n
          l@(_:_:_) -> error $ "getDeclInfo: Expected one data family instance for constructor " ++ show n ++ " but found multiple: " ++ show l
          [i] -> return (typeHead, instCons i)
      a -> error $ "getDeclInfo: Unmatched parent of data family instance: " ++ show a
  a -> error $ "getDeclInfo: Unmatched 'Info': " ++ show a

gadtIndices :: [Con] -> Q [Either Type Type]
gadtIndices constrs = fmap concat $ forM constrs $ \case
  GadtC _ _ (AppT _ typ) -> return [Right typ]
  ForallC _ _ (GadtC _ bts (AppT _ (VarT _))) -> fmap concat $ forM bts $ \case
    (_, AppT t (VarT _)) -> do
      hasArgDictInstance <- fmap (not . null) $ reifyInstances ''ArgDict [t]
      return $ if hasArgDictInstance then [Left t] else []
    _ -> return []
  ForallC _ _ (GadtC _ _ (AppT _ typ)) -> return [Right typ]
  _ -> return []
