-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.Test.QuantitiesSpec
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module PhyQ.Test.QuantitiesSpec (spec) where

import PhyQ.Test.Common
import PhyQ.Quantity

import PhysicalQuantities.Decomposition (tDecomposition)

-----------------------------------------------------------------------------

factor   = quantityInstance :: Factor
time     = quantityInstance :: Time
speed    = quantityInstance :: Speed
speedAbs = quantityInstance :: Abs Speed

-----------------------------------------------------------------------------

spec = describe "Physical Quantity" $ do
  it "has a name" $ do quantityName factor   `shouldBe` "Factor"
                       quantityName time     `shouldBe` "Time"
                       quantityName speed    `shouldBe` "Speed"
                       quantityName speedAbs `shouldBe` "|Speed|"

  it "defines dimensions" $ do
      quantityDimensions factor   `shouldBe` Dimensionless
      quantityDimensions time     `shouldBe` Scalar
      quantityDimensions speed    `shouldBe` Vector
      quantityDimensions speedAbs `shouldBe` Scalar

  it "can be decomposed to base quantities (including at type level)" $ do
      let speedD = [("Position", 1), ("Time", -1)]
      tDecomposition factor   `shouldBe` []
      tDecomposition time     `shouldBe` [("Time", 1)]
      tDecomposition speed    `shouldBe` speedD
      tDecomposition speedAbs `shouldBe` speedD
      tDecomposition (quantityInstance :: Abs Speed) `shouldBe` speedD

  it "..." $ do
      tDecomposition (factor :* time) `shouldBe` [("Time", 1)]
      tDecomposition (quantityInstance :: Factor :* Time ) `shouldBe` [("Time", 1)]
      -- tDecomposition (quantityInstance :: Speed :* Time )  `shouldBe` [("Position", 1)] -- TODO

  it "... can be combined and compared at type level ..." $ do
      mistake (B::B( EqQ Speed Acceleration ))
      mistake (B::B( EqQ Speed (Abs Speed) ))
      correct (B::B( EqQ Speed (Position :/ Time) ))
      correct (B::B( EqQ (Abs Position) Distance ))
      correct (B::B( EqQ (Abs Speed) (Distance :/ Time) ))

      correct (B::B( EqQ (Speed :* Time) Position ))

  it "can be converted to standard quantity representation" $ example pending

-----------------------------------------------------------------------------
