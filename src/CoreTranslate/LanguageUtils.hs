module CoreTranslate.LanguageUtils where

import CoreTranslate.Language
import GHC.Plugins
import GHC.Prelude
import CoreTranslate.Translate

isPrimitive :: VariableDef -> Bool
isPrimitive (Def n1 (Variable n2 _)) = n1 == n2
isPrimitive _ = False 

--panics if ProveName is in the current Module
getExternalVariableDef :: ProveName -> VariableDef
getExternalVariableDef (PN id) = Def (PN id) (convertExpression $ unfoldingTemplate $ realIdUnfolding id)

getVariableDef :: ProveLanguage -> ProveName -> Maybe VariableDef
getVariableDef (x:xs) n
    | defName x == n = Just x
    | otherwise = getVariableDef xs n
getVariableDef [] _ = Nothing