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
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable
import Data.Maybe
import Data.Monoid (Endo (..))
import Language.Haskell.TH

deriveGGeneric :: Name -> Q [Dec]
deriveGGeneric n = do
  ts <- gadtIndices n
  let xs :: [([(Int, Type)], Type)]
      xs = flip map ts $ fmap $ \case
        Left t -> AppT (ConT 'SExpr_List) $ AppT (ConT ''Indices) t
        Right t -> AppT (ConT 'SExpr_Atom) t
      types = foldr (AppT . AppT (ConT '(:))) (ConT '[]) $ fst <$> xs
  arity <- tyConArity n
  tyVars <- replicateM (arity - 1) (newName "a")
  let n' = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
  matchPairs <- matches n 'toIndex 'fromIndex
  [d| instance GGeneric $(pure n') where
        type Indices $(pure n') = $(pure types)
        toIndex = $(pure $ LamCaseE $ fst <$> matchPairs)
        fromIndex = $(pure $ LamCaseE $ snd <$> matchPairs)
    |]

matches :: Name -> Name -> Name -> Q [(Match, Match)]
matches n toIndexName fromIndexName = do
  x <- newName "x"
  reify n >>= \case
    TyConI (DataD _ _ _ _ constrs _) -> ifor constrs $ \idx -> \case
      GadtC [name] _ _ -> return
        ( Match (RecP name []) (NormalB $ succAppE idx atomE) []
        , Match (succAppP idx atomP) (NormalB $ RecE name []) []
        )
      ForallC _ _ (GadtC [name] bts (AppT _ (VarT b))) -> do
        ps <- forM bts $ \case
          (_, AppT t (VarT b')) | b == b' -> do
            hasGGenericInstance <- not . null <$> reifyInstances ''GGeneric [t]
            return $ if hasGGenericInstance
              then Just x
              else Nothing
          _ -> return Nothing
        return $ case catMaybes ps of
          [] -> pure
            ( Match (RecP name []) (NormalB $ succAppE idx atomE) []
            , Match (succAppP idx atomP) (NormalB $ RecE name []) []
            )
          (v:_) ->
            let patf = \v' rest done -> if done
                  then WildP : rest done
                  else case v' of
                    Nothing -> WildP : rest done
                    Just _ -> VarP v : rest True
                pat = foldr patf (const []) ps False
            in pure
              ( Match
                  (ConP name pat)
                  (NormalB $ succApp idx $ AppE list $ AppE (VarE toIndexName) $ VarE v)
                  []
              , Match
                  (ConP name pat)
                  (NormalB $ succApp idx $ AppE list $ AppE (VarE toIndexName) $ VarE v)
                  []
              )
      ForallC _ _ (GadtC [name] _ _) -> return
        ( Match (RecP name []) (NormalB $ succApp idx atomE) []
        , Match (RecP name []) (NormalB $ succApp aidxa atomE) []
        )
      a -> error $ "deriveGGeneric matches: Unmatched 'Dec': " <> show a
    a -> error $ "deriveGGeneric matches: Unmatched 'Info': " <> show a
    where
      succAppE idx = appEndo (fold $ succs idx) . AppE (ConE 'SExprCursor_Zero)
      succAppP idx = appEndo (fold $ succs idx) . AppP (ConP 'SExprCursor_Zero)
      succsE idx = replicate idx $ Endo $ AppE $ ConE 'SExprCursor_Succ
      succsP idx = replicate idx $ Endo $ AppP $ ConP 'SExprCursor_Succ
      atomE = ConE 'SExprCursorNode_Atom
      atomP = ConP 'SExprCursorNode_Atom
      listE = ConE 'SExprCursorNode_List
      listP = ConP 'SExprCursorNode_List

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

gadtIndices :: Name -> Q [([(Int, Type)], Either Type Type)]
gadtIndices n = reify n >>= \case
  TyConI (DataD _ _ _ _ constrs _) -> forM constrs $ \case
    GadtC _ _ (AppT _ typ) -> return $ Right typ
    ForallC _ _ (GadtC _ bts (AppT _ typ)) -> do
      bts' <- iforM bts $ \i bTyp -> do
        x <- MaybeT $ do
          (_, AppT t (VarT _)) <- pure bTyp
          (VarT _) <- pure typ
          hasGGenericInstance <- lift $ fmap (not . null) $ reifyInstances ''GGeneric [t]
          guard $ hasGGenericInstance
          pure t
        pure $ (,) i $ case x of
          Nothing -> Left typ
          Just t -> Right t
      let (gadts, normals) = partitionEithers bts'
      pure $ (,) normals $ case gadts of
        [] -> Right typ
        (gadtB : _) -> Left gadtB
    a -> error $ "gadtResults: Unmatched 'Dec': " <> show a
  a -> error $ "gadtResults: Unmatched 'Info': " <> show a
