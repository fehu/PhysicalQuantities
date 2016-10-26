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

-- * Base Units

$(genBaseUnit "Second")
$(genBaseUnit "Meter")
$(genBaseUnit "Kilogramm")
$(genBaseUnit "Kelvin")
$(genBaseUnit "Ampere")
$(genBaseUnit "Candela")
$(genBaseUnit "Mole")




