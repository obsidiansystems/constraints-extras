{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Constraint.Extras.TH {-# DEPRECATED "Use 'Data.GADT.Generics.TH' instead" #-} (deriveArgDict, deriveArgDictV, gadtIndices) where

import Data.GADT.Generics.TH
import Language.Haskell.TH

{-# DEPRECATED deriveArgDict "Just use 'deriveGGeneric'" #-}
deriveArgDict :: Name -> Q [Dec]
deriveArgDict = deriveGGeneric

{-# DEPRECATED deriveArgDictV "Just use 'deriveGGeneric'" #-}
deriveArgDictV :: Name -> Q [Dec]
deriveArgDictV = deriveArgDict
