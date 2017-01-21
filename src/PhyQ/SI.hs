-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.SI
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

module PhyQ.SI ( module Export ) where

import PhyQ.Quantity as Export
import PhyQ.Units.SI as Export

import PhysicalQuantities.Templates

-----------------------------------------------------------------------------

genUnitSystem "SI" Kilo [ Time      ~> Second
                        , Distance  ~> Meter
                        , Mass      ~> Kilogramm
--
--                        , Position  ~> Vec Meter
                        ]

--data SI = SI
--
--instance UnitSystem SI where
--    unitSystemName _ = "SI"
--    type Prefix SI = SIPrefix
--
--    type UnitFor SI Time     = Second
--    type UnitFor SI Distance = Meter
--    type UnitFor SI Mass     = Kilogramm
--
--    type UnitFor SI Position = Vec Meter


