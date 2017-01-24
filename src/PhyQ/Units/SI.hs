-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.Units.SI
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE UndecidableInstances #-}

module PhyQ.Units.SI ( SIPrefix(..), module Export ) where

import PhysicalQuantities.Definitions as Export

import PhyQ.Units.SI.Base    as Export
import PhyQ.Units.SI.Derived as Export

-----------------------------------------------------------------------------

data SIPrefix v = Atto | Femto | Pico | Nano | Micro | Milli | Centi | Deci
                | Deca | Hecto | Kilo | Mega | Giga  | Tera  | Peta  | Exa

    deriving (Show, Eq, Ord)


instance UnitPrefix SIPrefix where
    prefixGroup _ = "SIPrefix"
    prefixName = show
    prefixValue prefix = case prefix of  Atto     -> 10^(-18)
                                         Femto    -> 10^(-15)
                                         Pico     -> 10^(-12)
                                         Nano     -> 10^(-9)
                                         Micro    -> 10^(-6)
                                         Milli    -> 10^(-3)
                                         Centi    -> 10^(-2)
                                         Deci     -> 10^(-1)
                                         Deca     -> 10^1
                                         Hecto    -> 10^2
                                         Kilo     -> 10^3
                                         Mega     -> 10^6
                                         Giga     -> 10^9
                                         Tera     -> 10^12
                                         Peta     -> 10^15
                                         Exa      -> 10^16

    prefixFromValue v | v == 10^(-18) = Just Atto
                      | v == 10^(-15) = Just Femto
                      | v == 10^(-12) = Just Pico
                      | v == 10^(-9)  = Just Nano
                      | v == 10^(-6)  = Just Micro
                      | v == 10^(-3)  = Just Milli
                      | v == 10^(-2)  = Just Centi
                      | v == 10^(-1)  = Just Deci
                      | v == 10^1     = Just Deca
                      | v == 10^2     = Just Hecto
                      | v == 10^3     = Just Kilo
                      | v == 10^6     = Just Mega
                      | v == 10^9     = Just Giga
                      | v == 10^12    = Just Tera
                      | v == 10^15    = Just Peta
                      | v == 10^16    = Just Exa
                      | otherwise     = Nothing

    convertPrefix pref = case pref of  Atto     -> Atto
                                       Femto    -> Femto
                                       Pico     -> Pico
                                       Nano     -> Nano
                                       Micro    -> Micro
                                       Milli    -> Milli
                                       Centi    -> Centi
                                       Deci     -> Deci
                                       Deca     -> Deca
                                       Hecto    -> Hecto
                                       Kilo     -> Kilo
                                       Mega     -> Mega
                                       Giga     -> Giga
                                       Tera     -> Tera
                                       Peta     -> Peta
                                       Exa      -> Exa
