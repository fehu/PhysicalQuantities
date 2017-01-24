-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Combinations
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
           , UndecidableInstances
    #-}

module PhysicalQuantities.Combinations (
  (:*) (..)
, (:/) (..)
, (:^) (..)
) where


import PhysicalQuantities.Decomposition
import PhysicalQuantities.Decomposition.TStruct

import TypeNum.Rational

import Data.Type.Bool
import Data.Type.Equality

import Control.Arrow (second)


-----------------------------------------------------------------------------

data a :* b  = a :* b
data a :/ b  = a :/ b
data a :^ p  = a :^ Ratio' (AsRational p)

-----------------------------------------------------------------------------


instance (Decomposition a, Decomposition b) =>
  TDerived (a :* b) where
    type TStructure (a :* b) = SumTStructs (TDecomposition a) (TDecomposition b)


instance (Decomposition a, Decomposition b) =>
  TDerived (a :/ b) where
    type TStructure (a :/ b) = SumTStructs (TDecomposition a) (NegatePowers (TDecomposition b))


instance (Decomposition a, KnownRatio (AsRational p), NumValue (AsRational p) ~ Rational) =>
  TDerived (a :^ p) where
    type TStructure (a :^ p) = MultPowers (TDecomposition a) (AsRational p)


type instance DecompositionType (a :* b) = Derived
type instance DecompositionType (a :/ b) = Derived
type instance DecompositionType (a :^ p) = Derived


-----------------------------------------------------------------------------
