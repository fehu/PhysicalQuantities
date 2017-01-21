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
{-# OPTIONS_GHC -ddump-splices #-}

module PhyQ.Quantity.Base where

import PhysicalQuantities.Templates

-----------------------------------------------------------------------------




genQuantityBase "Factor" Dimensionless

genQuantityBase "Time"     Scalar
genQuantityBase "Distance" Scalar
genQuantityBase "Mass"     Scalar


genQuantityBase "Position" Vector

