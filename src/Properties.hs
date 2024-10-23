{-# LANGUAGE DeriveDataTypeable #-}
module Properties (
  Equiv(..),
  EquivCond(..),
  findEquivInLanguage
) where

import Prelude
import Data.Data
import CoreTranslate.Language
import Data.List (isPrefixOf)

data Equiv input result = Equiv (input -> result) (input -> result) deriving (Typeable)

data EquivCond input result = EquivCond (input -> Bool) (input -> result) (input -> result) deriving (Typeable)

findEquivInLanguage :: ProveLanguage -> [(ProveExpression, ProveExpression)]
findEquivInLanguage = concatMap findEquivInDef

-- as Equiv has 2 (type) parameters and 2 args it becomes Def _ (DO (DO (DO (DO EquivString) type1) type2) arg1) arg2) where type1 == input and type2 == output
findEquivInDef :: VariableDef -> [(ProveExpression, ProveExpression)]
findEquivInDef (Def _ (DirectOperation (DirectOperation (DirectOperation (DirectOperation (Variable equivName _) _ _) _ _) arg1 _) arg2 _)) = [(arg1, arg2) | "$my-prover-0.1.0.0-inplace$Properties$Equiv" `isPrefixOf` show equivName ]
findEquivInDef _ = []