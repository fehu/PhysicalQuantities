-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Decomposition.Quantity.Base
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module PhyQ.Quantity.Base where

import PhysicalQuantities.Templates

-----------------------------------------------------------------------------

data Factor = Factor
instance PhysicalQuantity Factor where
  type QuantityDimensions Factor = Dimensionless
  quantityName _ = "Factor"
  quantityDimensions _ = Dimensionless
  quantityInstance = Factor
instance TDerived Factor where
  type TStructure Factor = TStruct' '[]
type instance DecompositionType Factor = Derived

-----------------------------------------------------------------------------

-- genQuantityBase "Factor" Dimensionless

genQuantityBase "Time"     Scalar
genQuantityBase "Mass"     Scalar


genQuantityBase "Position" Vector
type Distance = Abs Position
