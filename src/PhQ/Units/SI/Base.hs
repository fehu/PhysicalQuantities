-----------------------------------------------------------------------------
--
-- Module      :  PhQ.Units.SI.Base
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell#-} -- , UndecidableInstances
-- {-# OPTIONS_GHC -ddump-splices #-}


module PhQ.Units.SI.Base where

import PhysicalQuantities.Templates
import PhQ.PhysicalQuantities

genUnitSystem "SI"

-- * Base Units

genBaseUnit "Second"
genBaseUnit "Meter"
genBaseUnit "Kilogramm"
genBaseUnit "Kelvin"
genBaseUnit "Ampere"
genBaseUnit "Candela"
genBaseUnit "Mole"

type instance UnitFor SI Time        = Second
type instance UnitFor SI Mass        = Kilogramm
type instance UnitFor SI Distance    = Meter
type instance UnitFor SI Temperature = Kelvin
--type instance UnitFor SI PlaneAngle =
--type instance UnitFor SI =

--type instance UnitFor SI =


