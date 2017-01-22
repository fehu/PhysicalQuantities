-----------------------------------------------------------------------------
--
-- Module      :  PhysicalQuantities.Decomposition.Quantity
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module PhyQ.Quantity (

  module Export
, TypesEq(..), type (=~=), type  (/~=), TypesOrd(..)

) where

import PhysicalQuantities.Definitions as Export

import PhyQ.Quantity.Base     as Export
import PhyQ.Quantity.Derived  as Export

import TypeNum.TypeFunctions

-----------------------------------------------------------------------------
