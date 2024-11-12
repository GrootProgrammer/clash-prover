{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

module TestModule where

import Properties
import Prelude
import Data.Int
import Data.Word

import           Data.Proxy
import           GHC.TypeLits
import qualified Clash.Prelude as CP

checkEasy :: Equiv Word32 Word32
checkEasy = Equiv (\i -> i + 1) (\i -> i - (maxBound @(Word32)))

check :: EquivMealy Int32 (Int32, Int32) (Int32, Int32, Int32) Int32
check = EquivMealy check1 (0,0) check2 (0,0,0)

check1 :: (Int32, Int32) -> Int32 -> ((Int32, Int32), Int32)
check1 (t1, t2) t0 = ((t0, t1), t0+t1+t2) 
check2 :: (Int32, Int32, Int32) -> Int32 -> ((Int32, Int32, Int32), Int32)
check2 (t1, t2, acc) t0 = ((t0, t1, acc + t0 - t2), acc + t0 - t2) 