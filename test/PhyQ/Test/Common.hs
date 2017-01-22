-----------------------------------------------------------------------------
--
-- Module      :  PhyQ.Test.Common
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE GADTs, PolyKinds #-}


module  PhyQ.Test.Common(

  correct, mistake
, B(..)

, module Test.Hspec

) where

import Test.Hspec

-----------------------------------------------------------------------------

data B (b :: Bool) = B


correct :: (expr ~ True) => c expr -> Expectation
correct _ = return ()

mistake :: (expr ~ False) => c expr -> Expectation
mistake _ = return ()
