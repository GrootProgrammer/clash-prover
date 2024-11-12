{-# LANGUAGE DeriveDataTypeable #-}
module Properties where

import Prelude
import Data.Data
import CoreTranslate.Language
import Data.Maybe

data Equiv input result = Equiv (input -> result) (input -> result) deriving (Typeable)

data EquivMealy input state1 state2 result = EquivMealy (state1 -> input -> (state1, result)) state1 (state2 -> input -> (state2, result)) state2 deriving (Typeable)

data EquivCond input result = EquivCond (input -> Bool) (input -> result) (input -> result) deriving (Typeable)

findEquivInLanguage :: ProveLanguage -> [VariableDef]
findEquivInLanguage = mapMaybe findEquivInDef

findEquivMealyInLanguage :: ProveLanguage -> [VariableDef]
findEquivMealyInLanguage = mapMaybe findEquivMealyInDef

isNameCall :: String -> ProveExpression -> Bool
isNameCall s (Variable exprVarName) = stableUnique exprVarName == s
isNameCall s (Lambda _ a) = isNameCall s a
isNameCall s (DirectOperation e _) = isNameCall s e
isNameCall _ _ = False


collect :: ProveExpression -> [ProveExpression]
collect (Literal         _ ) = []
collect (Variable        _ ) = []
collect (Lambda          e _) = []
collect (Case           _ _ _) = []
collect (DirectOperation e a) = a : collect e

extractEquivInfo :: ProveExpression -> (ProveExpression, ProveExpression, ProveExpression, ProveExpression)
extractEquivInfo def = (a,b,c,d)
    where
        a : b : c : d : _ = collect (def)

-- as Equiv has 2 (type) parameters and 2 args it becomes Def _ (DO (DO EquivString arg1) arg2) 
findEquivInDef :: VariableDef -> Maybe VariableDef
findEquivInDef def = if isNameCall "$my-prover-0.1.0.0-inplace$Properties$Equiv" (defExpr def) then Just def else Nothing

-- as Equiv has 2 (type) parameters and 2 args it becomes Def _ (DO (DO EquivString arg1) arg2) 
findEquivMealyInDef :: VariableDef -> Maybe VariableDef
findEquivMealyInDef def = if isNameCall "$my-prover-0.1.0.0-inplace$Properties$EquivMealy" (defExpr def) then Just def else Nothing