{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Invariants
  ( Invariants (..),
    GetInvariant (..),
    alwaysValid,
  )
where

import qualified Clash.Prelude as CP
import Prelude

newtype Invariants a = Invariant (a -> Bool)

alwaysValid :: Invariants a
alwaysValid = Invariant (const True)

class GetInvariant a where
  getInvariant :: Invariants a

instance GetInvariant CP.Bit where
  getInvariant = Invariant (\b -> (b == CP.low) || (b == CP.high))
