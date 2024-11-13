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

checkEasy :: Equiv (CP.Unsigned 32, CP.Unsigned 32) Bool
checkEasy = Equiv (\(a, b)-> a > b) (\(a, b) -> b <= a)