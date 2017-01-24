-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.Test.Units.SISpec
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module PhyQ.Test.Units.SISpec (spec) where

import PhyQ.Test.Common
import PhyQ.SI

import PhysicalQuantities.Decomposition (tDecomposition)

-----------------------------------------------------------------------------

spec = describe "Unit" $ do
  it "can be decomposed to base quantities (including at type level)" $ do
    tDecomposition (unitInstance :: Kilogramm) `shouldBe` [("Kilogramm", 1)]
    tDecomposition (unitInstance :: Newton)    `shouldBe` [ ("Kilogramm", 1)
                                                          , ("Meter", 1)
                                                          , ("Second", -2)
                                                          ]
  it "can be compared at type level" $ do
    correct (B::B( EqU Newton (Kilogramm:*Meter:/(Second:^2)) ))
    correct (B::B( EqU (Newton:*Second) (Kilogramm:*Meter:/Second) ))
    correct (B::B( EqU (Newton:/Kilogramm) (Meter:/(Second:^2)) ))
    correct (B::B( EqU (Meter:/Second:/Newton) (Second:/Kilogramm) ))
