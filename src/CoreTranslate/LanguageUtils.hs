module CoreTranslate.LanguageUtils where

import CoreTranslate.Language
import GHC.Plugins
import GHC.Prelude
import CoreTranslate.Translate
import CoreTranslate.Language (ProveLanguage, ProveExpression (Variable))

isPrimitive :: VariableDef -> Bool
isPrimitive (Def n1 (Variable n2)) = n1 == n2
isPrimitive _ = False

--panics if ProveName is in the current Module
getExternalVariableDef :: ProveName -> VariableDef
getExternalVariableDef (PN id) = Def (PN id) (convertExpression $ unfoldingTemplate $ realIdUnfolding id)

getVariableDef :: ProveLanguage -> ProveName -> VariableDef
getVariableDef (x:xs) n
    | defName x == n = x
    | otherwise = getVariableDef xs n
getVariableDef [] n = getExternalVariableDef n

evalToWHNF :: ProveLanguage -> [ProveName] -> ProveExpression -> ProveExpression
evalToWHNF _ _ (Literal l) = Literal l
evalToWHNF lang free (Variable n) = (Variable n)