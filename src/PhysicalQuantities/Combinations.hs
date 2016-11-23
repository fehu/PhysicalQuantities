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

import TypeNum.TypeFunctions
import TypeNum.Rational

import GHC.TypeLits (Symbol, CmpSymbol)
import GHC.Exts (sortWith)

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
    tStructure (a :* b) = mapByKeys (+) (tDecomposition a) (tDecomposition b)


instance (Decomposition a, Decomposition b) =>
  TDerived (a :/ b) where
    type TStructure (a :/ b) = SumTStructs (TDecomposition a) (NegatePowers (TDecomposition b))
    tStructure (a :/ b) = mapByKeys (-) (tDecomposition a) (tDecomposition b)


instance (Decomposition a, KnownRatio (AsRational p), NumValue (AsRational p) ~ Rational) =>
  TDerived (a :^ p) where
    type TStructure (a :^ p) = MultPowers (TDecomposition a) (AsRational p)
    tStructure (a :^ p) = second (*runtimeValue p) <$> tDecomposition a


type instance DecompositionType (a :* b) = Derived
type instance DecompositionType (a :/ b) = Derived
type instance DecompositionType (a :^ p) = Derived

-----------------------------------------------------------------------------

-- Sum associated list values

type family SumTStructs (m1 :: TStruct) (m2 :: TStruct) :: TStruct where
    SumTStructs (TStruct' m1) (TStruct' m2) = TStruct' (SumSortedMaps (QSort m1) (QSort m2))

type family SumSortedMaps (m1 :: [(k, p)]) (m2 :: [(k, p)]) :: [(k, p)] where
    SumSortedMaps  m1       '[]         = m1
    SumSortedMaps '[]        m2         = m2
    SumSortedMaps (h1 ': t1) (h2 ': t2) = SumSortedMaps' (CmpKeys h1 h2) (h1 ': t1) (h2 ': t2)

type family SumSortedMaps' (ord :: Ordering) (m1 :: [(k, p)]) (m2 :: [(k, p)]) :: [(k, p)] where
    SumSortedMaps' LT (h1 ': t1) m2         = h1 ': SumSortedMaps t1 m2
    SumSortedMaps' EQ (h1 ': t1) (h2 ': t2) = SumPows h1 h2 ': SumSortedMaps t1 t2
    SumSortedMaps' GT m1         (h2 ': t2) = h2 ': SumSortedMaps m1 t2


type family CmpKeys (x :: (k,p)) (y :: (k,p)) :: Ordering where
    CmpKeys '(k1,p1) '(k2,p2) = CmpSymbol k1 k2

type family SumPows (x :: (k,p)) (y :: (k,p)) :: (k,p) where
    SumPows '(k, p1) '(k, p2) = '(k, p1 + p2)

mapByKeys :: (Ord k, Num v) => (v -> v -> v) -> [(k,v)] -> [(k,v)] -> [(k,v)]
mapByKeys f l1 l2 = mapByKeys' f (sortWith fst l1) (sortWith fst l2)

mapByKeys' f l1              []                         = l1
mapByKeys' f []              l2                         = l2
mapByKeys' f l1@((k1,v1):t1) l2@((k2,v2):t2) | k1 < k2  = (k1, v1)      : mapByKeys' f t1 l2
                                             | k1 > k2  = (k2, v2)      : mapByKeys' f l1 t2
                                             | k1 == k2 = (k1, f v1 v2) : mapByKeys' f t1 t2

-----------------------------------------------------------------------------

-- List utils

type family (++) (l1 :: [a]) (l2 :: [a]) :: [a] where
    '[]      ++ l = l
    (h ': t) ++ l = h ': (t ++ l)


type family QSort (l :: [(k, p)]) :: [(k, p)] where
    QSort '[] = '[]
    QSort ('(u, n) ': t) = QSort (FilterOrd LT u t)
                         ++ '[ '(u, n) ]
                         ++ QSort (FilterOrd GT u t)

type family FilterOrd (ord :: Ordering) (e :: k) (l :: [(k, p)]) :: [(k, p)] where
    FilterOrd ord e '[] = '[]
    FilterOrd ord e ('(u, n) ': t) = If (CmpSymbol e u == ord ) ('(u, n) ': FilterOrd ord e t)
                                                              (FilterOrd ord e t)


type family MapValues f (l :: [(k, p)]) :: [(k, p)] where
    MapValues f '[]              = '[]
    MapValues f ( '(k, p) ': t ) = '(k, f :$: p) ': MapValues f t

-----------------------------------------------------------------------------

-- Power utits

type family NegatePowers (s :: TStruct) :: TStruct where
    NegatePowers (TStruct' m) = TStruct' (MapValues NegateFunc m)

type family MultPowers (s :: TStruct) (p :: TRational) :: TStruct where
    MultPowers (TStruct' m) p = TStruct' (MapValues (MultFunc p) m)


data NegateFunc (arg :: v) (res :: v)
type instance NegateFunc :$: v = Negate v

data MultFunc (p :: TRational) (arg :: v) (res :: v)
type instance (MultFunc p) :$: v = v*p

