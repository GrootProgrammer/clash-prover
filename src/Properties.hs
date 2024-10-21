{-# LANGUAGE DeriveDataTypeable #-}
module Properties (
  Equiv(..),
  EquivCond(..),
  findEquivInLanguage
) where

import Data.Data
import CoreTranslate.Language

data Equiv input result = Equiv (input -> result) (input -> result) deriving (Typeable)

data EquivCond input result = EquivCond (input -> Bool) (input -> result) (input -> result) deriving (Typeable)

findEquivInLanguage :: ProveLanguage -> [(ProveExpression, ProveExpression)]
findEquivInLanguage = concatMap findEquivInDef

findEquivInDef :: VariableDef -> [(ProveExpression, ProveExpression)]
findEquivInDef (Def _ (DirectOperation (DirectOperation (DirectOperation (DirectOperation (Variable equivName _) _ _) _ _) arg2 _) arg1 _)) = [(arg1, arg2)]
findEquivInDef _ = []