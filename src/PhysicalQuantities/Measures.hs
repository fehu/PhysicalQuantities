-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Measures
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module PhysicalQuantities.Measures(

  Measure(measuredValue, measuredPrefix), measured, measuredUnit
, SomePrefixFor(..)
, (:$)(..), ($:)

, mapMeasuredVal
, measuresSum, ($+)
, measuresSub, ($-)
, measuresMult, ($*)
, measuresDiv, ($/)

) where

import PhysicalQuantities.Definitions

import TypeNum.TypeFunctions

import Data.Function (on)

import Control.Arrow ( (&&&) )

-----------------------------------------------------------------------------

infixl 4 $:
infixl 6 $+, $-
infixl 7 $*, $/
infixl 9 :$

-----------------------------------------------------------------------------
-- * Definitions

-- | Measure shouldn't carry unit value, but rather its type.
data Measure v u = Measure {
    measuredValue  :: v -- ^ Raw value (without prefix), see 'measured'.
  , measuredPrefix :: Maybe (SomePrefixFor v)
  }

-- | Value with application of prefix, if any.
measured :: (Num v, Eq v) => Measure v u -> v
measured m = maybe (measuredValue m)
                   ((measuredValue m *) . prefixValue)
                   (measuredPrefix m)

-- | Materialized using 'unitInstance'.
measuredUnit :: (UnitDecomposition u) => Measure v u -> u
measuredUnit _ = unitInstance


instance TypesEq (Measure v u1) (Measure v u2) where
  type Measure v u1 ~=~ Measure v u2  = EqU u1 u2

instance TypesOrd (Measure v u1) (Measure v u2) where
  type Cmp (Measure v u1) (Measure v u2) = CmpU u1 u2

instance (Num v, Eq v) => Eq (Measure v u) where
  x == y =  f x == f y
         || measured x == measured y
      where f = measuredValue &&& measuredPrefix

instance (Num v, Ord v) => Ord (Measure v u) where
  compare = compare `on` measured

instance (Show v, UnitDecomposition u) => Show (Measure v u) where
  show m = show (measuredValue m) ++ " " ++ fullUnit
    where uname = unitName (measuredUnit m)
          fullUnit = maybe uname
                          (flip (++) $ "-" ++ uname)
                          (show <$> measuredPrefix m)

-----------------------------------------------------------------------------

-- | 'UnitPrefix' container for value type `v`.
data SomePrefixFor v = forall p . UnitPrefix p => SomePrefix (p v)
-- | Except 'prefixFromValue' method.
instance UnitPrefix SomePrefixFor where
  prefixGroup (SomePrefix p) = prefixGroup p
  prefixValue (SomePrefix p) = prefixValue p
  prefixName  (SomePrefix p) = prefixName p
  prefixFromValue = error $  "`prefixFromValue` cannot be called for" ++
                             "PhysicalQuantities.Measures.SomePrefixFor`"
  convertPrefix (SomePrefix p) = SomePrefix $ convertPrefix p

instance (Num v, Eq v) => Eq (SomePrefixFor v) where (==) = (==) `on` prefixValue
instance (Num v, Ord v) => Ord (SomePrefixFor v) where compare = compare `on` prefixValue
instance Show (SomePrefixFor v) where show (SomePrefix p) = prefixName p

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Creation

-- | Prefixed value constructor.
data (UnitPrefix p) => (:$) v p = (:$) v (p v)

-----------------------------------------------------------------------------

class MakeMeasure from
  where type MeasureValue from :: *
        -- | Measure constructor.
        ($:) :: from -> u -> Measure (MeasureValue from) u




class MakeMeasure' from (t :: MMT)
  where type MeasureValue' t from :: *
        mkMeasure' :: MMT' t -> from -> u -> Measure (MeasureValue' t from) u

-- | MakeMeasure Type
data MMT = MMPrefixed | MMUnprefixed
-- | 'MMT' proxy
data MMT' (t :: MMT) = MMT'

type family CanMakeMeasure (from :: *) :: MMT
  where CanMakeMeasure (v:$p) = MMPrefixed
        CanMakeMeasure  v     = MMUnprefixed

canMakeMeasure :: from -> MMT' (CanMakeMeasure from)
canMakeMeasure = const MMT'

-- | Creates measures using MakeMeasure' instances.
instance (MakeMeasure' from (CanMakeMeasure from)) => MakeMeasure from
  where type MeasureValue from = MeasureValue' (CanMakeMeasure from) from
        ($:) v = mkMeasure' (canMakeMeasure v) v


-- | Create unprefixed measure.
instance MakeMeasure' v MMUnprefixed where
  type MeasureValue' MMUnprefixed v = v
  mkMeasure' _ v _ = Measure v Nothing

-- | Create prefixed measure.
instance (UnitPrefix p) => MakeMeasure' (v:$p) MMPrefixed where
  type MeasureValue' MMPrefixed (v:$p) = v
  mkMeasure' _ (v:$p) _ = Measure v . Just $ SomePrefix p


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Operations

mapMeasuredVal :: (v -> w) -> Measure v u -> Measure w u
mapMeasuredVal f (Measure v p) = Measure (f v) (convertPrefix <$> p)


measuresSum :: (Num v, Ord v, True ~ EqU u1 u2) =>
              Measure v u1 -> Measure v u2 -> Measure v u1
measuresSum = dumbMeasuresOp (+)
measuresSub :: (Num v, Ord v, True ~ EqU u1 u2) =>
              Measure v u1 -> Measure v u2 -> Measure v u1
measuresSub = dumbMeasuresOp (-)
measuresMult :: (Num v, Ord v) =>
              Measure v u1 -> Measure v u2 -> Measure v (u1:*u2)
measuresMult = dumbMeasuresOp (*)
measuresDiv  :: (Fractional v, Ord v) =>
              Measure v u1 -> Measure v u2 -> Measure v (u1:/u2)
measuresDiv = dumbMeasuresOp (/)

-- | Alias for 'measuresSum'.
($+) :: (Num v, Ord v, True ~ EqU u1 u2) => Measure v u1 -> Measure v u2 -> Measure v u1
($+) = measuresSum
-- | Alias for 'measuresSub'.
($-) :: (Num v, Ord v, True ~ EqU u1 u2) => Measure v u1 -> Measure v u2 -> Measure v u1
($-) = measuresSub
-- | Alias for 'measuresMult'.
($*) :: (Num v, Ord v) => Measure v u1 -> Measure v u2 -> Measure v (u1:*u2)
($*) = measuresMult
-- | Alias for 'measuresDiv'.
($/) :: (Fractional v, Ord v) => Measure v u1 -> Measure v u2 -> Measure v (u1:/u2)
($/) = measuresDiv


-- Default operation implementaion: TODO: make it smarter
dumbMeasuresOp :: (Num v, Ord v) =>
                  (v -> v -> v) -> Measure v u1 -> Measure v u2 -> Measure v u
dumbMeasuresOp op (Measure v1 Nothing) (Measure v2 Nothing) = Measure (v1 `op` v2) Nothing
dumbMeasuresOp op (Measure v1 p1)      (Measure v2 p2)
              | p1 == p2  = Measure (v1 `op` v2) p1
              | otherwise = let f v = (*) v . maybe 1 prefixValue
                            in Measure ( f v1 p1 `op` f v2 p2 ) Nothing

-----------------------------------------------------------------------------
