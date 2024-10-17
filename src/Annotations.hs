{-# LANGUAGE DeriveDataTypeable #-}
module Annotations (
  Equiv(..),
  EquivCond(..)
) where

import Data.Data

data Equiv input result = Equiv (input -> result) (input -> result) deriving (Typeable)

data EquivCond input result = EquivCond (input -> Bool) (input -> result) (input -> result) deriving (Typeable)