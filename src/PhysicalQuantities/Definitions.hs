-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Definitions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FunctionalDependencies
           , FlexibleContexts
           , UndecidableInstances
           , PolyKinds
           , FlexibleInstances
       #-}

module PhysicalQuantities.Definitions (

  PhysicalQuantity(..), Dimensions(..)

, Unit(..), UnitSystem(..), UnitFor(..)
, (:*)(..), (:/)(..), (:^)(..)

, Measured(..), MeasurePrefix(..)
, Measurable(..)
--, measuredSys

, module TypeNum.Rational

) where

import TypeNum.Rational

-----------------------------------------------------------------------------

infixl 7 :*, :/
infixl 8 :^

-----------------------------------------------------------------------------


data Dimensions = Dimensionless | Scalar | Vector deriving (Show, Eq)

class PhysicalQuantity q where type QuantityDimensions q :: Dimensions
                               quantityName       :: q -> String


-----------------------------------------------------------------------------

class Unit u where unitName :: u -> String
                   unitInstance :: u

class UnitSystem sys where systemName :: sys -> String

type family UnitFor sys phq :: *

-----------------------------------------------------------------------------

data (Unit a, Unit b)           => a :* b  = (:*) a b
data (Unit a, Unit b)           => a :/ b  = (:/) a b
data (Unit a, MaybeRational p)  => a :^ p  = (:^) a (Ratio' (AsRational p))

instance (Unit a, Unit b) => Unit (a :* b) where
    unitName (a :* b) = unitName a ++ " * " ++ unitName b
    unitInstance = unitInstance :* unitInstance

instance (Unit a, Unit b) => Unit (a :/ b) where
    unitName (a :/ b) = unitName a ++ " / " ++ unitName b
    unitInstance = unitInstance :/ unitInstance

instance (Unit a, MaybeRational p, KnownRatio (AsRational p)) =>
    Unit (a :^ p) where
        unitName (a :^ p) = unitName a ++ "^" ++ show p
        unitInstance = unitInstance :^ Ratio'

-----------------------------------------------------------------------------

class ( MeasurePrefix (Prefix m) v
      , Eq m, Ord m, Show m  ) =>
  Measured m u v | m -> u, m -> v
    where type Prefix m
          measuredUnit     :: m -> u
          measuredPrefix   :: m -> Maybe (Prefix m)

          measuredValue :: m -> v
          measured  :: v -> Prefix m -> u -> m
          measured' :: v -> u -> m


class (Num v) => MeasurePrefix p v where prefixNum :: p -> v
                                         prefixFromNum :: v -> Maybe p


--unitFor :: Unit (UnitFor sys phq) => sys -> phq -> UnitFor sys phq
--unitFor _ _ = unitInstance
--
--measuredSys ::(Measured m u v, UnitFor sys q ~ u, Unit u) => sys -> q -> Prefix m -> v -> m
--measuredSys s q pref v = measured v pref (unitFor s q)



-----------------------------------------------------------------------------


--class Measurable m phq v | m -> phq, m -> v
--    where measurable   :: (MeasurePrefix pref v)             => phq -> pref -> v -> m
--          measurable'  ::                                       phq -> v -> m
--          inUnitSystem :: ( Measured m (UnitFor sys phq) v ) => m -> sys -> m0


data Measurable phq v

--inUnitSystem :: ( Measured m0 (UnitFor sys phq) v ) => Measurable phq v -> sys -> m0
--inUnitSystem =



