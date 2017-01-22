-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Decomposition
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleContexts
           , ExistentialQuantification
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
       #-}

module PhysicalQuantities.Decomposition  (

  TBase(..), TDerived(..)

, Decomposition(..)
, TStruct(..), TStructVal, CmpD
, DecompositionType(..), T(..)

) where

import PhysicalQuantities.Decomposition.TStruct

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))

import TypeNum.Rational (AsRational)

-----------------------------------------------------------------------------
-- * Public


class (KnownSymbol (TSymbol t)) =>
    TBase t where type TSymbol t  :: Symbol
                  tSymbol         :: t -> Proxy (TSymbol t)
                  tFromSymbol     :: Proxy (TSymbol t) -> t

                  tSymbol = const Proxy

class TDerived u where type TStructure u :: TStruct
                       tStructure :: u -> TStructVal


type TStructVal = [(String, Rational)]

-----------------------------------------------------------------------------
-- * Private

-- | Both 'base' and 'derived' types are decomposable.
class Decomposition t where type TDecomposition t :: TStruct
                            tDecomposition :: t -> TStructVal

data Decomposable = forall t . Decomposition t => Decomposable t

-- | Decomposition, depending on whether a type is a base or derived.
class Decomposition' t (tpe :: T) where type TDecomposition' t tpe :: TStruct
                                        tDecomposition' :: T' tpe -> t -> TStructVal

data T = Base | Derived
data T' (t :: T) = T'

type family DecompositionType a :: T

-----------------------------------------------------------------------------

decompositionType :: t -> T' (DecompositionType t)
decompositionType = const T'

-- | Decomposition is defined for any instance of `Decomposition'`.
instance (Decomposition' t (DecompositionType t)) => Decomposition t where
    type TDecomposition t = TDecomposition' t (DecompositionType t)
    tDecomposition t = tDecomposition' (decompositionType t) t


-- | Decomposition of a base type is its symbol in power 1.
instance (DecompositionType t ~ Base, TBase t) => Decomposition' t Base where
    type TDecomposition' t Base = TStruct' '[ '(TSymbol t, AsRational 1) ]
    tDecomposition' _ t = [(symbolVal $ tSymbol t, 1)]

-- | Decomposition of a derived type is defined in the corresponding `TDerived` instance.
instance (DecompositionType t ~ Derived, TDerived t) => Decomposition' t Derived where
    type TDecomposition' t Derived = TStructure t
    tDecomposition' _ = tStructure

-----------------------------------------------------------------------------

-- | Compare decompositions
type CmpD (a :: TStruct) (b :: TStruct) = CmpTStructs a b
