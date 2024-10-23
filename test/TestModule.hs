module TestModule where

import Prelude
import Properties

basicPlusEasy :: Equiv Integer Integer
basicPlusEasy = Equiv basicPlus easyPlus

easyPlus :: Integer -> Integer
easyPlus a = 15 + a

basicPlus :: Integer -> Integer
basicPlus input = 10 + fiveConstant + input

fiveConstant :: Integer
fiveConstant = 5