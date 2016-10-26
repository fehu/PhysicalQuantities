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

