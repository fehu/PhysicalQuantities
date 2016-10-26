-----------------------------------------------------------------------------
--
-- Module      :  PhQ.PhysicalQuantities
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module PhQ.PhysicalQuantities where

import PhysicalQuantities.Definitions
import PhysicalQuantities.Templates

-----------------------------------------------------------------------------

-- * Dimensionless
$(genPhysicalQuantity "Dimensionless" Dimensionless)


-- * Scalar
$(genPhysicalQuantity "Time" Scalar)
$(genPhysicalQuantity "Mass" Scalar)
$(genPhysicalQuantity "Distance" Scalar)
type Length = Distance
$(genPhysicalQuantity "Temperature" Scalar)
$(genPhysicalQuantity "PlaneAngle" Scalar)
type Angle = PlaneAngle
$(genPhysicalQuantity "SolidAngle" Scalar)


-- * Vector
$(genPhysicalQuantity "Position" Vector)
$(genPhysicalQuantity "Speed" Vector)
$(genPhysicalQuantity "Acceleration" Vector)
$(genPhysicalQuantity "Force" Vector)
$(genPhysicalQuantity "Impulse" Vector)

