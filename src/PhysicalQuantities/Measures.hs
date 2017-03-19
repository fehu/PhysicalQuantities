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
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}

module PhysicalQuantities.Measures(

  Measured(measuredValue, measuredPrefix)
, measured, measuredUnit
, Measurable(..), measurable, measurable'
, MeasurableVector(..)


, SomePrefixFor(..)
, measure, (:$)(..), MakeMeasure( ($:) )


, MeasuresOps(..), MeasuresFracOps(..)
, MeasureCoerce(..)

) where

import PhysicalQuantities.Definitions

import TypeNum.TypeFunctions

import Data.Function (on)
import Data.Typeable
import Data.Coerce

import Control.Arrow ( (&&&) )

-----------------------------------------------------------------------------

infixl 4 $:
infixl 6 $+, $-
infixl 7 $*, $/
infixl 9 :$

-----------------------------------------------------------------------------
-- * Definitions

-- | Measured shouldn't carry unit value, but rather its type.
data Measured u v = Measured {
    measuredValue  :: v -- ^ Raw value (without prefix), see 'measured'.
  , measuredPrefix :: Maybe (SomePrefixFor v)
  }


-- | Value with application of prefix, if any.
measured :: (Num v, Eq v) => Measured u v -> v
measured m = maybe (measuredValue m)
                   ((measuredValue m *) . prefixValue)
                   (measuredPrefix m)

-- | Materialized using 'unitInstance'.
measuredUnit :: (UnitDecomposition u) => Measured u v -> u
measuredUnit _ = unitInstance


instance TypesEq (Measured u1 v) (Measured u2 v) where
  type Measured u1 v ~=~ Measured u2 v  = EqU u1 u2

instance TypesOrd (Measured u1 v) (Measured u2 v) where
  type Cmp (Measured u1 v) (Measured u2 v) = CmpU u1 u2

instance (Num v, Eq v) => Eq (Measured u v) where
  x == y =  f x == f y
         || measured x == measured y
      where f = measuredValue &&& measuredPrefix

instance (Num v, Ord v) => Ord (Measured u v) where
  compare = compare `on` measured

instance (Show v, UnitDecomposition u) => Show (Measured u v) where
  show m = show (measuredValue m) ++ " " ++ fullUnit
    where uname = unitName (measuredUnit m)
          fullUnit = maybe uname
                          (flip (++) $ "-" ++ uname)
                          (show <$> measuredPrefix m)

instance Functor (Measured u) where
  fmap f (Measured v pref) = Measured (f v) (convertPrefix <$> pref)


-----------------------------------------------------------------------------

data Measurable q v = Measurable
    (forall sys u . (UnitSystem sys, u ~ UnitFor sys q) =>
                    sys -> Measured u v
    )

measurableQuantity :: (PhysicalQuantity q) => Measurable q v -> q
measurableQuantity _ = quantityInstance

instance TypesEq (Measurable q1 v) (Measurable q2 v) where
  type Measurable q1 v ~=~ Measurable q2 v  = EqQ q1 q2

instance TypesOrd (Measurable q1 v) (Measurable q2 v) where
  type Cmp (Measurable q1 v) (Measurable q2 v) = CmpQ q1 q2

instance (PhysicalQuantity q) => Show (Measurable q v) where
  show m = "Measurable " ++ quantityName (measurableQuantity m)

instance Functor (Measurable q) where
  fmap f (Measurable mf) = Measurable (fmap f . mf)

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
        -- | Measured constructor.
        ($:) :: from -> u -> Measured u (MeasureValue from)

measure :: v -> Measured u v
measure x = Measured x Nothing

measurable :: v -> Measurable q v
measurable x = Measurable $ const (measure x)

measurable' :: q -> v -> Measurable q v
measurable' = const measurable

class MakeMeasure' from (t :: MMT)
  where type MeasureValue' t from :: *
        mkMeasure' :: MMT' t -> from -> u -> Measured u (MeasureValue' t from)

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
  mkMeasure' _ v _ = Measured v Nothing

-- | Create prefixed measure.
instance (UnitPrefix p) => MakeMeasure' (v:$p) MMPrefixed where
  type MeasureValue' MMPrefixed (v:$p) = v
  mkMeasure' _ (v:$p) _ = Measured v . Just $ SomePrefix p


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Operations

class (Num v, Ord v) =>
  MeasuresOps m a1 a2 v where
    type EqF m a1 a2 :: Bool

    measuresSum  :: (True ~ EqF m a1 a2) => m a1 v -> m a2 v -> m a1 v
    measuresSub  :: (True ~ EqF m a1 a2) => m a1 v -> m a2 v -> m a1 v
    measuresMult :: m a1 v -> m a2 v -> m (a1:*a2) v

    -- | Alias for 'measuresSum'.
    ($+) :: (True ~ EqF m a1 a2) => m a1 v -> m a2 v -> m a1 v
    ($+) = measuresSum

    -- | Alias for 'measuresSub'.
    ($-) :: (True ~ EqF m a1 a2) => m a1 v -> m a2 v -> m a1 v
    ($-) = measuresSub

    -- | Alias for 'measuresMult'.
    ($*) :: m a1 v -> m a2 v -> m (a1:*a2) v
    ($*) = measuresMult


class (Fractional v, MeasuresOps m a1 a2 v) =>
  MeasuresFracOps m a1 a2 v where
    measuresDiv  :: m a1 v -> m a2 v -> m (a1:/a2) v

    -- | Alias for 'measuresDiv'.
    ($/) :: m a1 v -> m a2 v -> m (a1:/a2) v
    ($/) = measuresDiv


class MeasureCoerce m a0 a v where measureCoerce :: m a v -> m a0 v


class MeasurableVector vec v | vec -> v
  where
    measurableAbs  :: Measurable q vec -> Measurable (Abs q) v
    measurableNorm :: Measurable q vec -> Measurable q vec
    measurableScalarMult :: Measurable q1 vec -> Measurable q2 v -> Measurable (q1:*q2) vec

-----------------------------------------------------------------------------

instance (Num v, Ord v) => MeasuresOps Measured u1 u2 v where
  type EqF Measured u1 u2 = EqU u1 u2
  measuresSum  = dumbMeasuredOp (+)
  measuresSub  = dumbMeasuredOp (-)
  measuresMult = dumbMeasuredOp (*)

instance (Fractional v, Ord v) => MeasuresFracOps Measured u1 u2 v where
  measuresDiv = dumbMeasuredOp (/)


-- Default operation implementaion: TODO: make it smarter
dumbMeasuredOp :: (Num v, Ord v) =>
                  (v -> v -> v) -> Measured u1 v -> Measured u2 v -> Measured u v
dumbMeasuredOp op (Measured v1 Nothing) (Measured v2 Nothing) = Measured (v1 `op` v2) Nothing
dumbMeasuredOp op (Measured v1 p1)      (Measured v2 p2)
              | p1 == p2  = Measured (v1 `op` v2) p1
              | otherwise = let f v = (*) v . maybe 1 prefixValue
                            in Measured ( f v1 p1 `op` f v2 p2 ) Nothing

instance (EqU u0 u ~ True) =>
  MeasureCoerce Measured u0 u v where
    measureCoerce = coerce

-----------------------------------------------------------------------------

instance (Num v, Ord v) => MeasuresOps Measurable q1 q2 v where
  type EqF Measurable q1 q2 = EqQ q1 q2
  measuresSum  = dumbMeasurableOp (+)
  measuresSub  = dumbMeasurableOp (-)
  measuresMult = dumbMeasurableOp (*)

instance (Fractional v, Ord v) => MeasuresFracOps Measurable q1 q2 v where
  measuresDiv = dumbMeasurableOp (/)

-- Uses `dumbMeasuredOp`.
dumbMeasurableOp :: (Num v, Ord v) =>
                  (v -> v -> v) -> Measurable q1 v -> Measurable q2 v -> Measurable q v
dumbMeasurableOp op (Measurable mf1) (Measurable mf2) =
  Measurable $ \s -> dumbMeasuredOp op (mf1 s) (mf2 s)

instance (EqQ q0 q ~ True) =>
 MeasureCoerce Measurable q0 q v where
  measureCoerce (Measurable mf) = Measurable (coerce . mf)


-----------------------------------------------------------------------------
