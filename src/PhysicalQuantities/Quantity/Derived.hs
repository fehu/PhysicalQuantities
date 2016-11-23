-----------------------------------------------------------------------------
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
{-# OPTIONS_GHC -ddump-splices #-}

module PhysicalQuantities.Quantity.Derived where

import TypeNum.Rational
import PhysicalQuantities.Templates
import PhysicalQuantities.Quantity.Base

-----------------------------------------------------------------------------

genQuantityDerived "Speed"        (quantityInstance :: Position :/ Time)
genQuantityDerived "Acceleration" (quantityInstance :: Position :/ (Time:^(Pos 2:%1)))

