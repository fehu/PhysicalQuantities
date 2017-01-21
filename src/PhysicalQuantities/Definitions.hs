-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Definitions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE PolyKinds
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts
           , ExistentialQuantification
           , ConstraintKinds
           , UndecidableInstances
           , FlexibleInstances
       #-}

module PhysicalQuantities.Definitions (

  Dimensions (Scalar, Vector, Dimensionless)

, PhysicalQuantity(..), BaseQuantity, DerivedQuantity

, Unit(..), BaseUnit(..), DerivedUnit(..), Vec(..)

, UnitSystem(..), UnitPrefix(..), UnitS

, Measured(..),   measured
, Measurable(..), measuredS, measuredS'
, MeasuredS(..),  measuredSU

, (:*), (:/), (:^)

) where

import PhysicalQuantities.Combinations
import PhysicalQuantities.Decomposition (TBase, TDerived)

import TypeNum.Rational

-----------------------------------------------------------------------------

data Dimensions = Dimensionless | Scalar | Vector deriving (Show, Eq)

class PhysicalQuantity q where type QuantityDimensions q :: Dimensions
                               quantityDimensions :: q -> Dimensions
                               quantityName       :: q -> String
                               quantityInstance   :: q


type BaseQuantity    q = (PhysicalQuantity q, TBase    q)
type DerivedQuantity q = (PhysicalQuantity q, TDerived q)


-----------------------------------------------------------------------------


instance ( PhysicalQuantity a, PhysicalQuantity b ) =>
    PhysicalQuantity (a :* b) where
        type QuantityDimensions (a :* b) = ResultingDimensions' a b
        quantityDimensions      (a :* b) = resultingDimensions' a b
        quantityName            (a :* b) = quantityName' " * " a b
        quantityInstance = quantityInstance :* quantityInstance


instance ( PhysicalQuantity a, PhysicalQuantity b ) =>
    PhysicalQuantity (a :/ b) where
        type QuantityDimensions (a :/ b) = ResultingDimensions' a b
        quantityDimensions      (a :/ b) = resultingDimensions' a b
        quantityName            (a :/ b) = quantityName' " / " a b
        quantityInstance = quantityInstance :/ quantityInstance

instance ( PhysicalQuantity a, KnownRatio (AsRational p) ) =>
    PhysicalQuantity (a :^ p) where
        type QuantityDimensions (a :^ p) = QuantityDimensions a
        quantityDimensions      (a :^ p) = quantityDimensions a
        quantityName            (a :^ p) = "(" ++ quantityName a ++ ")^" ++ show p
        quantityInstance = quantityInstance :^ Ratio'



type family ResultingDimensions (a :: Dimensions) (b ::Dimensions) :: Dimensions where
    ResultingDimensions a             Dimensionless = a
    ResultingDimensions Dimensionless b             = b
    ResultingDimensions Scalar        Scalar        = Scalar
    ResultingDimensions a             b             = Vector

type ResultingDimensions' a b = ResultingDimensions (QuantityDimensions a)
                                                    (QuantityDimensions b)

quantityName' op a b = quantityName a ++ op ++ quantityName b

resultingDimensions a             Dimensionless = a
resultingDimensions Dimensionless b             = b
resultingDimensions Scalar        Scalar        = Scalar
resultingDimensions _             _             = Vector

resultingDimensions' a b = resultingDimensions (quantityDimensions a) (quantityDimensions b)

-----------------------------------------------------------------------------

-- | Represents a unit as a symbol (combination of symbols); measures no
--   specific 'PhysicalQuantity' outside of a 'UnitSystem'.
class Unit u (dim :: Dimensions) | u -> dim where unitName :: u -> String
                                                  unitInstance :: u

type BaseUnit    u dim = (Unit u dim, TBase    u)
type DerivedUnit u dim = (Unit u dim, TDerived u)

-- | Vector container for units
newtype Vec u = Vec u

-- | Vector container wraps any scalar unit and turns it into vector.
instance (Unit u Scalar) => Unit (Vec u) Vector where unitName u = "Vec[" ++ unitName u ++ "]"
                                                      unitInstance = Vec unitInstance

-----------------------------------------------------------------------------

instance ( Unit a dimA, Unit b dimB, dim ~ ResultingDimensions dimA dimB ) =>
    Unit (a :* b) dim where
        unitName (a :* b) = unitName' " * " a b
        unitInstance = unitInstance :* unitInstance

instance ( Unit a dimA, Unit b dimB, dim ~ ResultingDimensions dimA dimB ) =>
    Unit (a :/ b) dim where
        unitName (a :/ b) = unitName' " / " a b
        unitInstance = unitInstance :/ unitInstance

instance ( Unit a dim, KnownRatio (AsRational p) ) =>
    Unit (a :^ p) dim where
        unitName (a :^ p) = "(" ++ unitName a ++ ")^" ++ show p
        unitInstance = unitInstance :^ Ratio'

unitName' op a b = unitName a ++ op ++ unitName b

-----------------------------------------------------------------------------

-- | Establishes units for the physical quantities within the system.
class (UnitPrefix (Prefix sys)) =>
  UnitSystem sys where unitSystemName :: sys -> String
                       type Prefix sys :: * -> *
                       type UnitFor sys phq :: *

class UnitPrefix p where prefixGroup     :: p v -> String
                         prefixValue     :: (Num v, Eq v) => p v -> v
                         prefixFromValue :: (Num v, Eq v) => v   -> Maybe (p v)


-- | Represents a 'Unit' within a UnitSystem'.
type UnitS sys phq = ( PhysicalQuantity phq
                     , UnitSystem sys
                     , Unit (UnitFor sys phq) (QuantityDimensions phq)
                     )


-----------------------------------------------------------------------------

data Measured v u dim = forall p . (UnitPrefix p) => Measured v (Maybe (p v)) u

measured :: (Unit u dim, UnitPrefix p) => v -> p v -> u -> Measured v u dim
measured v p = Measured v (Just p)

--measured' :: (Unit u dim) => v        -> u -> Measured v u dim
--measured' v = Measured v noPrefix


-----------------------------------------------------------------------------

-- | Quantity value descriptor with no unit system specified.
data (PhysicalQuantity phq) => Measurable phq v = Measurable

-- | Quantity value within some unit system.
data MeasuredS v sys phq = forall u . ( UnitFor sys phq ~ u
                                      , Unit u (QuantityDimensions phq)
                                      , UnitPrefix (Prefix sys)
                                      ) =>
     MeasuredS v (Maybe (Prefix sys v)) u

measuredS :: UnitS sys phq => Measurable phq v -> sys -> Prefix sys v -> v -> MeasuredS v sys phq
measuredS _ _ pref v = MeasuredS v (Just pref) unitInstance

measuredS' :: UnitS sys phq => Measurable phq v -> sys -> v -> MeasuredS v sys phq
measuredS' _ _ v = MeasuredS v Nothing unitInstance

measuredSU :: ( UnitFor sys phq ~ u, Unit u (QuantityDimensions phq)
              , UnitPrefix (Prefix sys), Num v, Eq v ) =>
              Measurable phq v -> sys -> Measured v u (QuantityDimensions phq) -> MeasuredS v sys phq
measuredSU _ _ (Measured v p u) = case p
                                    of Nothing -> MeasuredS v Nothing u
                                       Just p' -> case prefixFromValue $ prefixValue p'
                                                    of Just pref -> MeasuredS v (Just pref) u
                                                       Nothing   -> error "incompatible unit prefix"

-----------------------------------------------------------------------------



-----------------------------------------------------------------------------


-- Test

--data Speed
--instance PhysicalQuantity Speed

--class Test1 t where speedA :: t a -> Measurable Speed a




