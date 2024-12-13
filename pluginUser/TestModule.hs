{-# LANGUAGE DataKinds #-}

module TestModule where

import qualified Clash.Prelude as CP
import Prelude
import Properties

{-# ANN checkEasy ("alwaysTrue", Ass []) #-}
checkEasy :: CP.Unsigned 32 -> CP.Unsigned 32 -> Bool
checkEasy a b = (a >= b) == (b < a)
