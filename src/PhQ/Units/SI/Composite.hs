-----------------------------------------------------------------------------
--
-- Module      :  PhQ.Units.SI.Composite
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

{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}


module PhQ.Units.SI.Composite where

import PhysicalQuantities.Templates

import PhQ.Units.SI.Base

-- * Composite Units

genDerivedUnit "MeterPerSecond" "mps" (unitInstance :: (Meter :/ Second))


