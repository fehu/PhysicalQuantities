-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Templates
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TemplateHaskell
           , ExistentialQuantification
           , FlexibleInstances
           , PolyKinds
           , UndecidableInstances
           , FlexibleContexts
       #-}

module PhysicalQuantities.Templates (

-- * Physical Quantities Code Generation
  genPhysicalQuantity

-- * Units Code Generation
, genBaseUnit
, genDerivedUnit

, module Export

) where

import Language.Haskell.TH

import PhysicalQuantities.Definitions as Export
import PhysicalQuantities.Decomposition as Export

import TypeNum.Rational

import GHC.TypeLits (SomeSymbol(..), symbolVal, Symbol)

import Data.Ratio

import Control.Arrow

-----------------------------------------------------------------------------


strNameConstFun fname name' = FunD (mkName fname)
                                   [ Clause [WildP] (NormalB (LitE $ StringL name')) []]

dataInstConstFun params fname name = FunD (mkName fname)
                                          [Clause params (NormalB $ ConE name) []]

tInst name tname body = TySynInstD (mkName tname) (TySynEqn [ConT name] body)

unitT name ut = do tName <- conT name
                   t     <- ut
                   return $ TySynInstD (mkName "UnitT'") (TySynEqn [tName] t)

-- Physical Quantities Code Generation
-----------------------------------------------------------------------------


genPhysicalQuantity :: String -> Dimensions -> Q [Dec]
genPhysicalQuantity name' dim = do
    let name = mkName name'
    dimType <- case dim of Dimensionless -> [t| Dimensionless |]
                           Scalar        -> [t| Scalar |]
                           Vector        -> [t| Vector |]
    sequence [ dataD (cxt []) name [] [] []
             , instanceD (cxt []) [t| PhysicalQuantity $(conT name)|]
                         [ return $ strNameConstFun "quantityName" name'
                         , return $ TySynInstD (mkName "QuantityDimensions")
                                               (TySynEqn [ConT name] dimType)
                         ]
             ]

-- Units Code Generation
-----------------------------------------------------------------------------


-- | Generates only data and 'Unit' instance.
genUnit' :: String -> Q [Dec]
genUnit' name' = let name = mkName name'
            in sequence [ dataD (cxt []) name [] [normalC name []] []
                        , instanceD (cxt []) [t| Unit $(conT name) |]
                                    [ return $ strNameConstFun "unitName" name'
                                    , return $ dataInstConstFun [] "unitInstance" name
                                    ]
                        ]

genBaseUnit :: String -> Q [Dec]
genBaseUnit name' = do let name = mkName name'
                       uDef <- genUnit' name'
                       iDef <- instanceD (cxt []) [t| BaseUnit $(conT name) |]
                                 [ return $ tInst name "UnitSymbol" (LitT (StrTyLit name'))
                                 , return $ dataInstConstFun [WildP] "unitFromSymbol" name
                                 ]
                       tDef <- unitT name [t| UnitBase |]
                       return $ uDef ++ [tDef, iDef]


genDerivedUnit :: (DerivedUnit u) => String -> u -> Q [Dec]
genDerivedUnit name' u = do let name = mkName name'
                            uDef <- genUnit' name'
                            struct <- structureAsType $ unitComposition u
                            iDef <- instanceD (cxt []) [t| DerivedUnit $(conT name) |]
                                              [ return $ tInst name "UnitComposition" struct ]
                            tDef <- unitT name [t| UnitDerived |]
                            return $ uDef ++ [tDef, iDef]


-----------------------------------------------------------------------------

structureAsType :: UnitStructVal -> TypeQ
structureAsType s = [t| UnitStruct' $(f sTypes) |]
    where sTypes = map (\(s,r) -> [t| '( $(litT (strTyLit s)), $(rationalAsType r)) |]) s
          f (x:xs) = [t| $x ': $(f xs) |]
          f []     = promotedNilT


rationalAsType :: Rational -> TypeQ
rationalAsType r = let n = numerator r
                       npref = if n < 0 then [t| Neg |] else [t| Pos |]
                       num = [t| $npref $(litT . numTyLit $ (abs n)) |]
                       den = litT . numTyLit $ denominator r
                   in  [t| $num :% $den |]


