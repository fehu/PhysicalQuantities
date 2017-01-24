-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.Test.Measures.SISpec
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module PhyQ.Test.Measures.SISpec (spec) where

import PhyQ.Test.Common
import PhyQ.SI
import PhysicalQuantities.Measures

-----------------------------------------------------------------------------

spec = describe "Measures" $ do
  it "is constructed from some value, a unit and possibly a prefix" $ do
      3:$Kilo$:Meter `shouldBe` (3000 :: Int)$:Meter
      show ((3 :: Int)$:Meter) `shouldBe` "3 Meter"
      show (3.2:$Nano$:Meter) `shouldBe` "3.2 Nano-Meter"
      show ((3 :: Int)$:Meter:/Second) `shouldBe` "3 (Meter / Second)"

  it "contains value and its unit" $ example pending
  it "can be compared by unit at type-level" $ example pending
  it "can be converted to standard unit representation" $ example pending
  it "can be compared by value if units are the same" $ example pending
