module Prove.MealyProver where

import CoreTranslate.Language
import Debug.Trace
import Execute.Simplify

-- input type, state type, result type, trans, initial
data MealyMachine = MM ProveExpression ProveExpression ProveExpression ProveExpression ProveExpression
    deriving Show

-- mealy 1, mealy 2
data PairMealyMachine = PMM MealyMachine MealyMachine
    deriving Show

extractMealyContent :: VariableDef -> PairMealyMachine
extractMealyContent (Def _ (DirectOperation (DirectOperation (DirectOperation (DirectOperation (DirectOperation (DirectOperation (DirectOperation (DirectOperation (Variable _) initial2) trans2) initial1) trans1) result) state2) state1) input )) =
    PMM (MM input state1 result trans1 initial1) (MM input state2 result trans2 initial2)
extractMealyContent _ = error "not a mealy machine equiv"

simplifyMealy :: ProveLanguage -> MealyMachine -> MealyMachine
simplifyMealy lang (MM input state result trans initial) = MM (simplify lang [] input) (trace ("finished simplifying input: " ++ show (simplify lang [] state)) $ simplify lang [] state) (traceShowId $ simplify lang [] result) (traceShowId $ simplify lang [] trans) (traceShowId $ simplify lang [] initial)
simplifyMealys :: ProveLanguage -> PairMealyMachine -> PairMealyMachine
simplifyMealys lang (PMM one two) = PMM (simplifyMealy lang one) (simplifyMealy lang two)

proveMealy :: ProveLanguage -> VariableDef -> IO Bool
proveMealy lang mealy = traceShow (simplifyMealys lang $ extractMealyContent mealy) pure True