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


{-# LANGUAGE FunctionalDependencies
           , FlexibleContexts
           , ConstraintKinds
           , UndecidableInstances
           , FlexibleInstances
       #-}

module PhysicalQuantities.Definitions (

  Dimensions (Scalar, Vector, Dimensionless)

, PhysicalQuantity(..), BaseQuantity, DerivedQuantity, Abs(..)
, CmpQ, EqQ

, Unit(..), BaseUnit(..), DerivedUnit(..), Vec(..)

, UnitSystem(..), UnitPrefix(..), UnitS

, (:*)(..), (:/)(..), (:^)(..)

) where

import PhysicalQuantities.Combinations
import PhysicalQuantities.Decomposition ( TBase(..), TDerived(..)
                                        , Decomposition(..), DecompositionType
                                        , CmpD )

import TypeNum.Rational hiding (Abs)

import GHC.TypeLits (symbolVal)

import Data.Type.Bool (If)
import Data.Type.Equality

-----------------------------------------------------------------------------

data Dimensions = Dimensionless | Scalar | Vector deriving (Show, Eq, Ord)

instance TypesEq  (a :: Dimensions) (b :: Dimensions)
  where type a ~=~ b  = CompareDimensions a b == EQ
instance TypesOrd (a :: Dimensions) (b :: Dimensions)
  where type Cmp a b  = CompareDimensions a b

type instance (a :: Dimensions) == (b :: Dimensions)  =  a ~=~ b

type family CompareDimensions (a :: Dimensions) (b :: Dimensions) :: Ordering
  where CompareDimensions Dimensionless Dimensionless = EQ
        CompareDimensions Dimensionless Scalar = LT
        CompareDimensions Dimensionless Vector = LT
        CompareDimensions Scalar Dimensionless = GT
        CompareDimensions Vector Dimensionless = GT

        CompareDimensions Scalar Scalar = EQ
        CompareDimensions Scalar Vector = LT
        CompareDimensions Vector Scalar = GT
        CompareDimensions Vector Vector = EQ

-----------------------------------------------------------------------------


class PhysicalQuantity q where type QuantityDimensions q :: Dimensions
                               quantityDimensions :: q -> Dimensions
                               quantityName       :: q -> String
                               quantityInstance   :: q


type BaseQuantity    q = (PhysicalQuantity q, TBase    q)
type DerivedQuantity q = (PhysicalQuantity q, TDerived q)

type QuantityDecomposition q = (PhysicalQuantity q, Decomposition q)

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

instance ( PhysicalQuantity a, MaybeRational p, KnownRatio (AsRational p) ) =>
    PhysicalQuantity (a :^ (p :: k)) where
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

-- | Scalar container for vector quantities.
newtype Abs q = Abs q

instance (PhysicalQuantity a, QuantityDimensions a ~ Vector) =>
  PhysicalQuantity (Abs a) where
    type QuantityDimensions (Abs a) = Scalar
    quantityDimensions _ = Scalar
    quantityName (Abs a) = "|" ++ quantityName a ++ "|"
    quantityInstance = Abs quantityInstance

type instance DecompositionType (Abs a) = DecompositionType a

instance (BaseQuantity a) => TBase (Abs a) where
  type TSymbol (Abs a) = TSymbol a
  tFromSymbol = Abs . tFromSymbol

instance (DerivedQuantity a) => TDerived (Abs a) where
  type TStructure (Abs a) = TStructure a
  tStructure (Abs a) = tStructure a

-----------------------------------------------------------------------------

-- | Represents a unit as a symbol (combination of symbols); measures no
--   specific 'PhysicalQuantity' outside of a 'UnitSystem'.
class Unit u (dim :: Dimensions) | u -> dim where unitName :: u -> String
                                                  unitInstance :: u

type BaseUnit    u dim = (Unit u dim, TBase    u)
type DerivedUnit u dim = (Unit u dim, TDerived u)

type UnitDecomposition u dim = (Unit u dim, Decomposition u)

-- | Vector container for scalar units.
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

instance ( Unit a dim, MaybeRational p, KnownRatio (AsRational p) ) =>
    Unit (a :^ (p :: k)) dim where
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
-----------------------------------------------------------------------------
-- eq and compare for quantities and units, using decompositions.

class ( QuantityDecomposition q1, QuantityDecomposition q2 ) =>
  CompareQuantities q1 q2 where
    type CmpQ q1 q2 :: Ordering
    type EqQ  q1 q2 :: Bool

    type EqQ q1 q2 = CmpQ q1 q2 == EQ

-- | Compare physical quantities by dimensions and decomposition.
instance ( QuantityDecomposition q1, QuantityDecomposition q2 ) =>
  CompareQuantities q1 q2 where
    type CmpQ q1 q2 = If (QuantityDimensions q1 == QuantityDimensions q2)
                         (CmpD (TDecomposition q1)     (TDecomposition q2))
                         (Cmp  (QuantityDimensions q1) (QuantityDimensions q2))

-----------------------------------------------------------------------------

class ( UnitDecomposition u1 d1, UnitDecomposition u2 d2 ) =>
  CompareUnits u1 d1 u2 d2 | u1 -> d1, u2 -> d2 where

-----------------------------------------------------------------------------
