-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Decomposition.TStruct
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- | TODO : FilterZeroPow immediately
--

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module PhysicalQuantities.Decomposition.TStruct (

  TStruct(..)

, (++), QSort
, MapPairs, MapValues, MapKeys, mapByKeys
, MapTStruct, MapTStructKeys, MapTStructVals

, SumTStructs, CmpTStructs
, NegatePowers, MultPowers

) where

import Data.Type.Bool

import GHC.Exts (sortWith)
import GHC.TypeLits (Symbol, KnownSymbol, CmpSymbol, symbolVal)

import TypeNum.TypeFunctions
import TypeNum.Rational

-----------------------------------------------------------------------------

data TStruct  = TStruct' [(Symbol, TRational)]

-----------------------------------------------------------------------------
-- Utils for [..] and ((.., ..))

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

type family MapPairs f (l :: [(k, p)]) :: [(k, p)] where
    MapPairs f '[]         = '[]
    MapPairs f ( kp ': t ) = (f :$: kp) ': MapPairs f t

type family MapKeys f (l :: [(k, p)]) :: [(k, p)] where
    MapKeys f '[]             = '[]
    MapKeys f( '(k, p) ': t ) = '(f :$: k, p) ': MapKeys f t

-----------------------------------------------------------------------------
-- Map TStruct values and keys

type family MapTStruct f (t :: TStruct) where
    MapTStruct f (TStruct' m) = TStruct' (MapPairs f m)

type family MapTStructKeys f (t :: TStruct) where
    MapTStructKeys f (TStruct' m) = TStruct' (MapKeys f m)

type family MapTStructVals f (t :: TStruct) where
    MapTStructVals f (TStruct' m) = TStruct' (MapValues f m)

-----------------------------------------------------------------------------
-- Sum association list values

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

-----------------------------------------------------------------------------
-- Compare association lists.

type family CmpTStructs (s1 :: TStruct) (s2 :: TStruct) :: Ordering where
  CmpTStructs (TStruct' m1) (TStruct' m2) = CmpSortedMaps (QSort (FilterZeroPow m1))
                                                          (QSort (FilterZeroPow m2))

type family CmpSortedMaps (m1 :: [(k, p)]) (m2 :: [(k, p)]) :: Ordering where
  CmpSortedMaps '[] '[]  = EQ
  CmpSortedMaps  m1 '[]  = GT
  CmpSortedMaps '[]  m2  = LT
  CmpSortedMaps ( '(k1,p1) ': t1) ( '(k2,p2) ': t2 ) =
    If (k1 \= k2)
       (Cmp k1 k2)
       (
        If (p1 \= p2)
           (Cmp p1 p2)
           (CmpSortedMaps t1 t2)
       )


type (\=) a b = Not (a == b)

type family FilterZeroPow (m :: [(k, p)]) :: [(k, p)] where
    FilterZeroPow '[] = '[]
    FilterZeroPow ( '(k,p) ': t ) = If (p ~=~ 0)            (FilterZeroPow t)
                                                 ( '(k,p) ': FilterZeroPow t)

-----------------------------------------------------------------------------

mapByKeys :: (Ord k, Num v) => (v -> v -> v) -> [(k,v)] -> [(k,v)] -> [(k,v)]
mapByKeys f l1 l2 = mapByKeys' f (sortWith fst l1) (sortWith fst l2)

mapByKeys' f l1              []                         = l1
mapByKeys' f []              l2                         = l2
mapByKeys' f l1@((k1,v1):t1) l2@((k2,v2):t2) | k1 < k2  = (k1, v1)      : mapByKeys' f t1 l2
                                             | k1 > k2  = (k2, v2)      : mapByKeys' f l1 t2
                                             | k1 == k2 = (k1, f v1 v2) : mapByKeys' f t1 t2


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
