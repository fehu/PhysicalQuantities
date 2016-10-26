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
       #-}

module PhysicalQuantities.Definitions (

  PhysicalQuantity(..), Dimensions(..)

, Unit(..), UnitSystem(..), UnitFor(..)
, (:*)(..), (:/)(..), (:^)(..)

, Measured(..)

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

class Measured m u v | m -> u, m -> v
    where type Prefix m
          measuredUnit     :: m -> u
          measuredPrefix   :: m -> Maybe (Prefix m)
          measuredRawValue :: m -> v

          measuredValue :: m -> v
          measured  :: v -> Prefix m -> u -> m
          measured' :: v -> u -> m

-----------------------------------------------------------------------------


