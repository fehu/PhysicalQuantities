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
-- |
--

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PhysicalQuantities.Decomposition.TStruct (

-- * Type Level

  TStruct(..)

, (++), QSort
, MapPairs, MapValues, MapKeys
, FilterZeroPow, SortAndFilterZeroPow

, MapTStruct, MapTStructKeys, MapTStructVals

, SumTStructs, CmpTStructs
, NegatePowers, MultPowers

-- * Value level

, TStructVal, KnownTStruct (materializeTStruct)
) where

import Data.Type.Bool
import Data.Proxy

import GHC.Exts (sortWith)
import GHC.TypeLits (Symbol, KnownSymbol, CmpSymbol, symbolVal)

import TypeNum.TypeFunctions
import TypeNum.Rational

-----------------------------------------------------------------------------

data TStruct  = TStruct' [(Symbol, TRational)]


-----------------------------------------------------------------------------

instance TypesEq  (s1 :: Symbol) (s2 :: Symbol) where type s1 ~=~ s2 = CmpSymbol s1 s2 == EQ
instance TypesOrd (s1 :: Symbol) (s2 :: Symbol) where type Cmp s1 s2 = CmpSymbol s1 s2

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
    FilterOrd ord e ('(u, n) ': t) = If (Cmp u e == ord ) ('(u, n) ': FilterOrd ord e t)
                                                          (FilterOrd ord e t)

-----------------------------------------------------------------------------

type family FilterZeroPow (m :: [(k, p)]) :: [(k, p)] where
    FilterZeroPow '[] = '[]
    FilterZeroPow ( '(k,p) ': t ) = If (p ~=~ 0)            (FilterZeroPow t)
                                                 ( '(k,p) ': FilterZeroPow t)

type SortAndFilterZeroPow m = QSort (FilterZeroPow m)

-----------------------------------------------------------------------------

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

-- |  Sum association list values
type family SumTStructs (s1 :: TStruct) (s2 :: TStruct) :: TStruct
  where SumTStructs (TStruct' m1) (TStruct' m2) =
          TStruct' (SumTStructLists (QSort m1) (QSort m2))

type family SumTStructLists (s1 :: [(k,v)]) (s2 :: [(k,v)]) :: [(k,v)]
  where SumTStructLists ( '(k1,v1) ': t1 ) ( '(k2,v2) ': t2 ) =
            If (k1 == k2) ( '(k1, v1+v2) ': SumTStructLists t1 t2 )
            (If (k1 < k2) ( '(k1, v1) ': SumTStructLists t1 ( '(k2,v2) ': t2 ))
                          ( '(k2, v2) ': SumTStructLists ( '(k1,v1) ': t1 ) t2 ))

        SumTStructLists '[] s2 = s2
        SumTStructLists s1 '[] = s1


-----------------------------------------------------------------------------
-- Compare association lists.

type family CmpTStructs (s1 :: TStruct) (s2 :: TStruct) :: Ordering where
  CmpTStructs (TStruct' m1) (TStruct' m2) = CmpSortedMaps (SortAndFilterZeroPow m1)
                                                          (SortAndFilterZeroPow m2)

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


-----------------------------------------------------------------------------

type TStructVal = [(String, Rational)]



class KnownTStruct (t :: TStruct) where
  materializeTStruct :: Proxy (t :: TStruct) -> TStructVal

instance (KnownStructPairVals m) => KnownTStruct (TStruct' m) where
  materializeTStruct = structPairVals . tStructList



class KnownStructPairVals (m :: [(Symbol, TRational)]) where
  structPairVals :: Proxy m -> [(String, Rational)]

instance KnownStructPairVals '[] where structPairVals = const []
instance (KnownSymbol (Fst p), KnownRatio (Snd p), KnownStructPairVals t) =>
  KnownStructPairVals ( p ': t ) where
     structPairVals p = let par = structPairVal $ listHead p
                        in par : structPairVals (listTail p)


listHead :: Proxy ( h ': t ) -> Proxy h
listHead = const Proxy

listTail :: Proxy ( h ': t ) -> Proxy t
listTail = const Proxy


type family TStructList (t :: TStruct) :: [(Symbol, TRational)]
  where TStructList (TStruct' m) = m

tStructList :: Proxy (t :: TStruct) -> Proxy (TStructList t)
tStructList = const Proxy

structPairVal :: (KnownSymbol (Fst x), KnownRatio (Snd x)) =>
                Proxy (x :: (Symbol, TRational)) -> (String, Rational)
structPairVal p = let (ps, pr) = structPairProxies p
                  in (symbolVal ps, runtimeValue pr)

structPairProxies :: Proxy (x :: (Symbol, TRational)) -> (Proxy (Fst x), Ratio' (Snd x))
structPairProxies _ = (Proxy, Ratio')
