-----------------------------------------------------------------------------
--
-- Module      :  PhQ.Units.SI
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
           , ConstraintKinds
       #-}


module PhQ.Units.SI ( module Export, MeasuredSI ) where


import PhysicalQuantities.Definitions as Export
import PhQ.Units.SI.Base      as Export
import PhQ.Units.SI.Composite as Export

import Data.Char (toLower)

-----------------------------------------------------------------------------

data SIPrefix = Atto | Femto | Pico | Nano | Micro | Milli | Centi | Deci
              | Deca | Hecto | Kilo | Mega | Giga  | Tera  | Peta  | Exa

    deriving (Show, Eq, Ord)

instance (Fractional frac) => MeasurePrefix SIPrefix frac where
    prefixNum prefix = case prefix of  Atto     -> 10^(-18)
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

-----------------------------------------------------------------------------

type UnitSI phq u = (Unit u, u ~ UnitFor SI phq)

data (UnitSI phq u) => MeasuredSI phq u v = MeasuredSI u (Maybe SIPrefix) v

withPrefix (MeasuredSI _ mbPref v)= pref*v
    where pref = maybe 1 prefixNum mbPref


instance (UnitSI phq u) => Eq (MeasuredSI phq u Rational) where
    m1@(MeasuredSI _ p1 v1) == m2@(MeasuredSI _ p2 v2) =
        (p1 == p2 && v1 == v2) ||
        withPrefix m1 == withPrefix m2

instance (UnitSI phq u) => Ord (MeasuredSI phq u Rational) where
    m1@(MeasuredSI _ p1 v1) `compare` m2@(MeasuredSI _ p2 v2) =
        withPrefix m1 `compare` withPrefix m2

instance (UnitSI phq u) => Show (MeasuredSI phq u Rational) where
    show (MeasuredSI u p v) = show v ++ " " ++ pref ++ "-" ++ unitName u
        where pref = toLower (head pref') : tail pref'
              pref' = show p

instance (UnitSI phq u) => Measured (MeasuredSI phq u Rational) u Rational where
    type Prefix (MeasuredSI phq u Rational) = SIPrefix

    measuredUnit   (MeasuredSI u p v) = u
    measuredPrefix (MeasuredSI u p v) = p
    measuredValue  (MeasuredSI u p v) = v
    measured v p u = MeasuredSI u (Just p) v
    measured' v u  = MeasuredSI u Nothing  v

-----------------------------------------------------------------------------


