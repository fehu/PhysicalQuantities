-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Decomposition.Templates
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE TemplateHaskell
--           , ExistentialQuantification
--           , FlexibleInstances
--           , PolyKinds
--           , UndecidableInstances
--           , FlexibleContexts
       #-}

module PhysicalQuantities.Templates (

  genQuantityBase, genQuantityDerived

, module Export

) where

import PhysicalQuantities.Definitions   as Export
import PhysicalQuantities.Decomposition as Export

import TypeNum.Rational

import Language.Haskell.TH

import Control.Monad

import Data.Ratio
--import Data.Traversable (for)

-----------------------------------------------------------------------------

noCxt = cxt []

addDecls ::DecsQ -> [Dec] -> DecsQ
addDecls dsq ds = fmap (++ ds) dsq

-- Decomposition Atoms (Bases) Generation
-----------------------------------------------------------------------------

-- | data $name = $name
--   instance
_genQuantity :: Name -> Dimensions -> DecsQ
_genQuantity name dim = do
    let dimType = case dim of Dimensionless -> [t| Dimensionless |]
                              Scalar        -> [t| Scalar |]
                              Vector        -> [t| Vector |]
    qData <- dataD noCxt name [] [normalC name []] []
    qInst <- instanceD noCxt [t|PhysicalQuantity $(conT name)|]
                             [ tySynInstD (mkName "QuantityDimensions")
                                          (tySynEqn [conT name] dimType)
                             , funD (mkName "quantityName")
                                    [clause [wildP] (normalB . litE $ stringL (nameBase name)) [] ]
                             , funD (mkName "quantityDimensions")
                                    [clause [wildP] (normalB $ conE (mkName $ show dim)) [] ]
                             , funD (mkName "quantityInstance")
                                    [clause [] (normalB $ conE name) []]
                             ]
    return [qData, qInst]

_decompositionType :: Name -> T -> DecQ
_decompositionType name t = tySynInstD (mkName "DecompositionType")
                                       (tySynEqn [conT name] tt)
    where tt = case t of Base    -> conT $ mkName "Base"
                         Derived -> conT $ mkName "Derived"

genQuantityBase :: String -> Dimensions -> Q [Dec]
genQuantityBase nme dim = do
    let name = mkName nme

    tBaseInst <- instanceD noCxt [t|TBase $(conT name)|]
                                 [ tySynInstD (mkName "TSymbol")
                                              (tySynEqn [conT name] . litT $ strTyLit nme)
                                 , funD (mkName "tFromSymbol")
                                        [clause [wildP] (normalB $ conE name) []]
                                 ]
    dType <- _decompositionType name Base
    _genQuantity name dim `addDecls` [tBaseInst, dType]


genQuantityDerived :: (DerivedQuantity q) => String -> q -> Q [Dec]
genQuantityDerived nme q = do
    let name = mkName nme

    tDerivedInst <- instanceD noCxt
                              [t|TDerived $(conT name)|]
                              [ tySynInstD (mkName "TStructure")
                                           (tySynEqn [conT name] $ structTypeQ q)
                              , funD (mkName "tStructure")
                                     [clause [wildP] (normalB $ structValQ q) []]
                              ]
    dType <- _decompositionType name Derived
    _genQuantity name (quantityDimensions q) `addDecls` [tDerivedInst, dType]



-----------------------------------------------------------------------------

structTypeQ :: (TDerived t) => t -> TypeQ
structTypeQ t = [t|TStruct' $(struct2Type $ tStructure t)|]

struct2Type :: TStructVal -> TypeQ
struct2Type = foldr f promotedNilT <=< struct2Types
    where f t acc = [t| $(return t) ': $acc |]

struct2Types :: TStructVal -> Q[Type]
struct2Types = mapM $ \(s,p) -> [t|'($(litT $ strTyLit s), $(ratioType p))|]


ratioType r = [t|$num :% $(nLit $ denominator r)|]
    where nLit = litT . numTyLit
          num = case numerator r of n | n < 0 -> [t|Neg $(nLit n)|]
                                    n         -> [t|Pos $(nLit n)|]


structValQ :: (TDerived t) => t -> ExpQ
structValQ = listE . map f . tStructure
    where f (s,p) = [|( $(litE $ stringL s), $(litE $ rationalL p) )|]

