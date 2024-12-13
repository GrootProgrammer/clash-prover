{-# LANGUAGE DeriveDataTypeable #-}

module Properties where

import Data.Typeable 
import Data.Data

newtype AssertList
    = Ass [VariableProperty]
    deriving (Typeable, Data, Show)

data VariableProperty
    = Total String
    | CompleteTotal String
    deriving (Typeable, Data, Show)