-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.Units.SI.Base
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}


module PhyQ.Units.SI.Base where

import PhysicalQuantities.Templates

-----------------------------------------------------------------------------

genUnitBase "Second"
genUnitBase "Meter"
genUnitBase "Kilogramm"
