{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TestModule where

import GHC.TypeLits
import Data.Proxy
import CorePrintPlugin

data Unsigned (n :: Nat) = US Integer

mkUnsigned :: forall n. (KnownNat n) => Integer -> Unsigned n
mkUnsigned a = US (a `mod` (2 ^ (natVal (Proxy :: Proxy n))))

addUS :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
addUS (US a) (US b) = mkUnsigned (a + b)

addUSO :: forall n m. (KnownNat n, KnownNat m, m ~ n + 1) => Unsigned n -> Unsigned n -> Unsigned m
addUSO (US a) (US b) = mkUnsigned (a + b)

zeroN = 0
zero :: Unsigned 1
zero = US zeroN

ge :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Bool
ge (US a) (US b) = a >= b

{-# RULES
"first eq" forall a b. addUS a b = if ge a b then addUS a b else addUS a b
#-}