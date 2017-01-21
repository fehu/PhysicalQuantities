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
           , ExistentialQuantification
--           , FlexibleInstances
--           , PolyKinds
--           , UndecidableInstances
           , FlexibleContexts
       #-}

module PhysicalQuantities.Templates (

  genQuantityBase, genQuantityDerived
, genUnitBase, genUnitDerived
, genUnitSystem, (~>)

, module Export

) where

import PhysicalQuantities.Definitions   as Export
import PhysicalQuantities.Decomposition as Export

import TypeNum.Rational as Export

import Language.Haskell.TH

import Control.Monad

import Data.Ratio
--import Data.Traversable (for)





-- Quantities atoms (bases) generation.
-----------------------------------------------------------------------------

_genQuantity :: Name -> Dimensions -> DecsQ
_genQuantity name dim = do
    let dimType = case dim of Dimensionless -> [t| Dimensionless |]
                              Scalar        -> [t| Scalar |]
                              Vector        -> [t| Vector |]
    qData <- dataD noCxt name [] Nothing [normalC name []] noCxt
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


-- | Generate an atomic quantity.
genQuantityBase :: String -> Dimensions -> Q [Dec]
genQuantityBase nme dim = do
    let name = mkName nme
    _genQuantity name dim `mergeDecls` _genBase name

-- | Generate a derived quantity.
genQuantityDerived :: (DerivedQuantity q) => String -> q -> Q [Dec]
genQuantityDerived nme q = do
    let name = mkName nme
    _genQuantity name (quantityDimensions q) `mergeDecls` _genDerived name q



-- Units generation.
-----------------------------------------------------------------------------

_genUnit :: Name -> DecsQ
_genUnit name = do
    uData <- dataD noCxt name [] Nothing [normalC name []] noCxt
    uInst <- instanceD noCxt [t|Unit $(conT name) Scalar|]
                       [ funD (mkName "unitName")
                              [clause [wildP] (normalB . litE . stringL $ nameBase name) []]
                       , funD (mkName "unitInstance")
                              [clause [] (normalB $ conE name) []]
                       ]

    return [uData, uInst]


-- | Generate a /scalar/ base unit.
genUnitBase :: String -> DecsQ
genUnitBase nme = do
    let name = mkName nme
    _genUnit name `mergeDecls` _genBase name


-- | Generate a /scalar/ derived unit.
genUnitDerived :: DerivedUnit u Scalar => String -> u -> DecsQ
genUnitDerived nme u = do
    let name = mkName nme
    _genUnit name `mergeDecls` _genDerived name u


-- Unit systems generation.
-----------------------------------------------------------------------------

data UnitAssignation = forall q u dim. (PhysicalQuantity q, Unit u dim) => UnitAssignation q u

a ~> b = UnitAssignation a b

genUnitSystem :: (UnitPrefix prefix) => String -> prefix v -> [UnitAssignation] -> DecsQ
genUnitSystem nme pref as = do
    let name = mkName nme
        prefType = conT . mkName $ prefixGroup pref
        unitAssignations = do UnitAssignation q u <- as
                              let qt = conT . mkName $ quantityName q
                                  ut = conT . mkName $ unitName u
                              return . tySynInstD (mkName "UnitFor")
                                     $ tySynEqn [conT name, qt] ut
    sData <- dataD noCxt name [] Nothing [normalC name []] noCxt
    sInst <- instanceD noCxt [t|UnitSystem $(conT name)|]
                     $ [ funD (mkName "unitSystemName")
                              [clause [wildP] (normalB . litE $ stringL nme) []]
                       , tySynInstD (mkName "Prefix") $
                                    tySynEqn [conT name] prefType
                       ]
                    ++ unitAssignations
    return [sData, sInst]

-----------------------------------------------------------------------------

-- Generate Base/Derived part of definition/

noCxt = cxt []

addDecls :: DecsQ -> [Dec] -> DecsQ
addDecls dsq ds = fmap (++ ds) dsq

mergeDecls :: DecsQ -> DecsQ -> DecsQ
mergeDecls aq bq =  (++) <$> aq <*> bq


_genBase :: Name -> DecsQ
_genBase name = do
    tBaseInst <- instanceD noCxt [t|TBase $(conT name)|]
                                 [ tySynInstD (mkName "TSymbol")
                                              (tySynEqn [conT name] . litT . strTyLit $ nameBase name)
                                 , funD (mkName "tFromSymbol")
                                        [clause [wildP] (normalB $ conE name) []]
                                 ]
    dType <- _decompositionType name Base

    return [tBaseInst, dType]

_genDerived :: TDerived t => Name -> t -> DecsQ
_genDerived name t = do
    tDerivedInst <- instanceD noCxt
                              [t|TDerived $(conT name)|]
                              [ tySynInstD (mkName "TStructure")
                                           (tySynEqn [conT name] $ structTypeQ t)
                              , funD (mkName "tStructure")
                                     [clause [wildP] (normalB $ structValQ t) []]
                              ]
    dType <- _decompositionType name Derived
    return [tDerivedInst, dType]

-----------------------------------------------------------------------------
-- TStruct utils

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


-----------------------------------------------------------------------------
