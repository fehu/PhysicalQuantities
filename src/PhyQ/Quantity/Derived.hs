----------------------------------------;-------------------------------------
--
-- Module      :  PhysicalQuantities.Decomposition.Quantity.Derived
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module PhyQ.Quantity.Derived where

import TypeNum.Rational
import PhysicalQuantities.Templates
import PhyQ.Quantity.Base

-----------------------------------------------------------------------------

genQuantityDerived "Speed"        (quantityInstance :: Position :/ Time)
genQuantityDerived "Acceleration" (quantityInstance :: Position :/ (Time:^2))
genQuantityDerived "Force"        (quantityInstance :: Mass :* Position :/ (Time:^2))
