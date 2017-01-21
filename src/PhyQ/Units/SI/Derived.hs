-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.Units.SI.Derived
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, UndecidableInstances #-}

module PhyQ.Units.SI.Derived where

import PhysicalQuantities.Templates
import PhyQ.Units.SI.Base

-----------------------------------------------------------------------------

genUnitDerived "Newton" (unitInstance :: Kilogramm :* Meter :/ (Second:^Pos 2))
