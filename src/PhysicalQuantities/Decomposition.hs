-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Decomposition
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE PolyKinds
           , UndecidableInstances
           , ConstraintKinds
           , FlexibleInstances
           , MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleContexts
       #-}

module PhysicalQuantities.Decomposition

--(
--  UnitDecomposition(..), SomeUnit(..)
--
--, BaseUnit(..), DerivedUnit(..)
--
--, UnitT(..), UnitT'(..)
--
--, UnitStruct(..), UnitStructVal
--
--)

where


import PhysicalQuantities.Definitions

import Data.Type.Bool

import GHC.TypeLits (Symbol, KnownSymbol, CmpSymbol, SomeSymbol(..), symbolVal, someSymbolVal)
import Data.Proxy
import Data.List (nub)
import Data.Maybe (fromMaybe)

import Control.Arrow

import TypeNum.TypeFunctions
import TypeNum.Integer
import TypeNum.Rational


-----------------------------------------------------------------------------

class (Unit u, KnownSymbol (UnitSymbol u)) =>
    BaseUnit u where type UnitSymbol u  :: Symbol
                     unitSymbol         :: u -> Proxy (UnitSymbol u)
                     unitFromSymbol     :: Proxy (UnitSymbol u) -> u

                     unitSymbol = const Proxy
                     unitFromSymbol = const unitInstance


data UnitStruct  = UnitStruct' [(Symbol, TRational)]

type UnitStructVal = [(String, Rational)]

class (Unit u) => DerivedUnit u where type UnitComposition u :: UnitStruct
                                      unitComposition :: u -> UnitStructVal

--                                      unitComposition u = unitStructVal (Proxy :: Proxy (UnitComposition u))

-----------------------------------------------------------------------------

class (Unit u) => UnitDecomposition u where type UnitStructure u :: UnitStruct
                                            unitStructure :: u -> UnitStructVal

data SomeUnit = forall u . UnitDecomposition u => SomeUnit u

-----------------------------------------------------------------------------

data UnitT = UnitBase | UnitDerived

data AUnitT (t :: UnitT) = AUnitT

aUnitT :: u -> AUnitT (UnitT' u)
aUnitT = const AUnitT

type family UnitT' u :: UnitT

type instance UnitT' (a :* b) = UnitDerived
type instance UnitT' (a :/ b) = UnitDerived
type instance UnitT' (a :^ b) = UnitDerived

-----------------------------------------------------------------------------

class (Unit u) => UnitDecomposition' u (t :: UnitT) where
    type UnitStructure' u (t :: UnitT) :: UnitStruct
    unitStructure' :: u -> AUnitT t -> UnitStructVal


-- | Provides decomposition for all 'BaseUnit's.
instance (BaseUnit u, UnitT' u ~ UnitBase) => UnitDecomposition' u UnitBase where
    type UnitStructure' u UnitBase = UnitStruct' '[ '(UnitSymbol u, AsRational 1) ]
    unitStructure' u _ = [(symbolVal (unitSymbol u), 1)]

-- | Proxies decomposition for all 'DerivedUnit's.
instance (DerivedUnit u, UnitT' u ~ UnitDerived) => UnitDecomposition' u UnitDerived where
    type UnitStructure' u UnitDerived = UnitComposition u
    unitStructure' u _ = unitComposition u

---- | Is defined using the two 'UnitDecomposition'' instances.
--instance (Unit u, UnitDecomposition' u (UnitT' u)) => UnitDecomposition u where
--    type UnitStructure u = UnitStructure' u (UnitT' u)
--    unitStructure u = unitStructure' u (aUnitT u)


-- | Is defined using the two 'UnitDecomposition'' instances to distinguish base and derived units.
instance (Unit u, UnitDecomposition' u (UnitT' u)) => UnitDecomposition u where
    type UnitStructure u = UnitStructure' u (UnitT' u)
    unitStructure u = unitStructure' u (aUnitT u)


-----------------------------------------------------------------------------

-- TODO: sum powers by key, as in 'sumUnitStructs'
type family (++) (l1 :: [(k, p)]) (l2 :: [(k, p)]) :: [(k, p)] where
    '[]      ++ l = l
    (h ': t) ++ l = h ': (t ++ l)


type family SumUnitStruct (l1 :: UnitStruct) (l2 :: UnitStruct) :: UnitStruct where
    SumUnitStruct (UnitStruct' a) (UnitStruct' b) = UnitStruct' (a ++ b)


sumUnitStructs :: UnitStructVal -> UnitStructVal -> UnitStructVal
sumUnitStructs s1 s2 =
    let allKeys = nub $ map fst (s1 ++ s2)
        val k s = fromMaybe 0 $ lookup k s
        sum' k  = (k, val k s1 + val k s2)
    in map sum' allKeys


type family NegatePowers' (l :: [(k, p)]) :: [(k, p)] where
    NegatePowers' '[] = '[]
    NegatePowers' ('(u, x) ': t) = '(u, Negate x) ': NegatePowers' t

negatePowers :: UnitStructVal -> UnitStructVal
negatePowers = map (second negate)

type family NegatePowers struct where
    NegatePowers (UnitStruct' s) = UnitStruct' (NegatePowers' s)


type family MultPowers' (l :: [(k, TRational)]) (p :: TRational) :: [(k, TRational)] where
    MultPowers' '[] p = '[]
    MultPowers' ('(u, n) ': t) p = '(u, n*p) ': MultPowers' t p

type family MultPowers l r where
    MultPowers (UnitStruct' s) p = UnitStruct' (MultPowers' s p)


multPowers :: UnitStructVal -> Rational -> UnitStructVal
multPowers xs p = map (second (*p)) xs


-----------------------------------------------------------------------------


type family DecompositionEq (a :: [(k, p)]) (b :: [(k, p)]) :: Bool where
    DecompositionEq '[] '[] = True
    DecompositionEq ('(ua, ia) ': ta) ('(ub, ib) ': tb) = ua == ub &&
                                                          ia == ib &&
                                                          DecompositionEq ta tb

-----------------------------------------------------------------------------

type family FilterOrd (ord :: Ordering) (e :: k) (l :: [(k, p)]) :: [(k, p)] where
    FilterOrd ord e '[] = '[]
    FilterOrd ord e ('(u, n) ': t) = If (CmpSymbol e u == ord ) ('(u, n) ': FilterOrd ord e t)
                                                              (FilterOrd ord e t)


type family TQSort (l :: [(k, p)]) :: [(k, p)] where
    TQSort '[] = '[]
    TQSort ('(u, n) ': t) = TQSort (FilterOrd LT u t)
                         ++ '[ '(u, n) ]
                         ++ TQSort (FilterOrd GT u t)


type family StructEq (a :: UnitStruct) (b :: UnitStruct) :: Bool where
    StructEq (UnitStruct' '[]) (UnitStruct' '[]) = True
    StructEq (UnitStruct' as)  (UnitStruct' bs)  = DecompositionEq (TQSort as) (TQSort bs)


-----------------------------------------------------------------------------

instance (Unit a, Unit b) => TypesEq a b where
    type a ~~ b = StructEq (UnitStructure a) (UnitStructure b)

-----------------------------------------------------------------------------

instance (UnitDecomposition a, UnitDecomposition b) =>
  DerivedUnit (a :* b) where
    type UnitComposition (a :* b) = SumUnitStruct (UnitStructure a)
                                                  (UnitStructure b)
    unitComposition (a :* b) = sumUnitStructs (unitStructure a) (unitStructure b)

instance (UnitDecomposition a, UnitDecomposition b) =>
  DerivedUnit (a :/ b) where
    type UnitComposition (a :/ b) = SumUnitStruct (UnitStructure a)
                                                  (NegatePowers (UnitStructure b))

    unitComposition (a :/ b) = sumUnitStructs (unitStructure a) (negatePowers $ unitStructure b)

instance (UnitDecomposition a, MaybeRational p, KnownRatio (AsRational p)) =>
  DerivedUnit (a :^ p) where
    type UnitComposition (a :^ p) = MultPowers (UnitStructure a) (AsRational p)
    unitComposition (a :^ p) = multPowers (unitStructure a) (runtimeValue p)


-----------------------------------------------------------------------------



-----------------------------------------------------------------------------

class NonemptyUnitStruct (s :: UnitStruct) where  type StructHead s :: (Symbol, TRational)
                                                  type StructTail s :: UnitStruct

                                                  structHead :: Proxy s -> Proxy (StructHead s)
                                                  structTail :: Proxy s -> Proxy (StructTail s)

                                                  structHead = const Proxy
                                                  structTail = const Proxy

instance NonemptyUnitStruct (UnitStruct' (x ': xs)) where
    type StructHead (UnitStruct' (x ': xs)) =  x
    type StructTail (UnitStruct' (x ': xs)) = UnitStruct' xs


proxyFst :: Proxy (p :: (a,b)) -> Proxy (Fst p)
proxyFst = const Proxy

proxySnd :: Proxy p -> Proxy (Snd p)
proxySnd = const Proxy


class UnitStructValue (struct :: UnitStruct) where
    unitStructVal :: Proxy struct -> UnitStructVal

proxy2Ratio :: Proxy x -> Ratio' x
proxy2Ratio Proxy = Ratio'

proxyRatioValue :: (KnownRatio x) => Proxy (x :: TRational) -> Rational
proxyRatioValue = runtimeValue . proxy2Ratio


instance (KnownSymbol s, KnownRatio r, UnitStructValue (UnitStruct' xs)) =>
  UnitStructValue (UnitStruct' ('(s, r) ': xs)) where
    unitStructVal proxy = headValue (structHead proxy)
                        : unitStructVal (structTail proxy)
        where headValue p = (symbolVal $ proxyFst p, proxyRatioValue $ proxySnd p)

instance UnitStructValue (UnitStruct' '[]) where unitStructVal _ = []

-----------------------------------------------------------------------------
