-----------------------------------------------------------------------------
--
-- Module      :  PhQ.TestSI
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

module Main where

import PhQ.Test.Common
import PhQ.Units.SI


main = hspec $ do
    describe "'MeterPerSecond' unit" $ do
        it "equals (Meter :/ Second) unit"
           $ correct(B::B( MeterPerSecond ~~ (Meter :/ Second) ))
          && mistake(B::B( MeterPerSecond ~~ Meter ))
          && mistake(B::B( MeterPerSecond ~~ (Meter :* Second) ))

--    describe "Measured SI Value" $ do
--        it "can be constructed with a prefix or not"
--        it ""
--            $ speedM1 `inUnitSystem` SI == measured' 5 MeterPerSecond
--            $ measuredSys SI Speed Nothing (5 :: Float) == measured' 5 MeterPerSecond


--    where speedM1 = measurable' Speed (5 :: Float)


speedM :: Measurable Speed Float
speedM = undefined



