-----------------------------------------------------------------------------
--
-- Module      :  PhQ.Test.Common
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--


module PhQ.Test.Common(

  correct, mistake
, B(..)

--, module TypeNum
, module Test.Hspec


)   where

import Test.Hspec


-----------------------------------------------------------------------------

data B (b :: Bool) = B


correct :: (expr ~ True) => c expr -> Bool
correct _ = True

mistake :: (expr ~ False) => c expr -> Bool
mistake _ = True



