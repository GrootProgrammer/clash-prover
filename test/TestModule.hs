module TestModule where

import Prelude
import Properties
import GHC.Integer

negateIdentity :: Equiv Integer Integer
negateIdentity = Equiv (negateInteger) negateBySub

negateBySub :: Integer -> Integer
negateBySub a = 0 - a 