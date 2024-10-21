module TestModule where

import Properties

basicPlusEasy :: Equiv Integer Integer
basicPlusEasy = Equiv basicPlus (+ 15) 

basicPlus :: Integer -> Integer
basicPlus input = 10 + fiveConstant + input

fiveConstant :: Integer
fiveConstant = 5