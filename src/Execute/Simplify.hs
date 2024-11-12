{-# LANGUAGE BlockArguments #-}
module Execute.Simplify where

import CoreTranslate.Language
import CoreTranslate.LanguageUtils
import GHC.Core (AltCon(..))
import GHC (DataCon)
import GHC.Natural (Natural)
import Execute.Primitives (primitiveExecute, isPrimitive)
import GHC.Core.DataCon (dataConWorkId)
import Debug.Trace (trace)

isDataConMatching :: DataCon -> ([ProveExpression], ProveExpression) -> Bool
isDataConMatching dc (_, Literal (Dict lc _)) = dc == lc
isDataConMatching _ _ = False

isSameElseCall :: Eq a => a -> a -> (a -> a) -> a
isSameElseCall a b f = if a == b then a else f b

iterateUntilId :: Eq a => (a -> a) -> a -> a
iterateUntilId funcParam start = isSameElseCall start (funcParam start) (iterateUntilId funcParam)
iterateUntilLeft :: (a -> Either a a) -> a -> a
iterateUntilLeft funcParam start =
    case funcParam start of
        Left _ -> start
        Right e -> iterateUntilLeft funcParam e

simplify :: ProveLanguage -> [ProveExpression] -> ProveExpression -> ProveExpression
simplify lang stack = iterateUntilLeft (simplifyStep lang stack [])

resolveStack :: ProveLanguage -> [ProveName] -> Natural -> [ProveExpression] -> [ProveExpression]
resolveStack _ _ 0 xs = xs
resolveStack lang free n (x:xs) = case x_s of
    Left _ -> x:resolveStack lang free (n-1) xs
    Right x_simp -> x_simp:xs
    where
        x_s = simplifyStep lang [] free x
resolveStack _ _ _ [] = error "stack not populated"

simplifyStepCases :: ProveLanguage -> [ProveName] -> [CaseInstance] -> Either [CaseInstance] [CaseInstance]
simplifyStepCases _ _ [] = Left []
simplifyStepCases lang free ((CI con binds caseExpr):xs) = case case_simplified of
    Left _ -> fmap (CI con binds caseExpr:) (simplifyStepCases lang free xs)
    Right e -> Right $ CI con binds e:xs
    where
        case_simplified = simplifyStep lang [] (binds ++ free) caseExpr

getConstructorBinding :: ProveExpression -> Maybe (ProveName, [ProveExpression])
getConstructorBinding (Literal (Constructor d)) = Just (d, [])
getConstructorBinding (DirectOperation e a) = fmap (\(c, args) -> (c, args ++ [a])) (getConstructorBinding e)
getConstructorBinding _ = Nothing

matchCase :: ProveExpression -> [CaseInstance] -> Maybe ProveExpression
matchCase _ [] = Nothing
-- dictionary lookup, mostly used for instances like Num
matchCase (Literal (Dict con1 binds1)) ((CI (DataAlt con2) binds2 caseExpr):xs)
    | con1 == con2 = Just $ foldr (\(to, from) e -> replaceVariable e from to) caseExpr $ zip (tail binds1) binds2
    | otherwise = matchCase (Literal (Dict con1 binds1)) xs
-- constructor matching
matchCase match ((CI (DataAlt con2) binds2 caseExpr):xs) = case getConstructorBinding match of
    Nothing -> Nothing
    Just (PN con1, binds1) ->
        (if (con1 == dataConWorkId con2) && (length binds1 == length binds2) then Just $ foldr (\(to, from) e -> replaceVariable e from to) caseExpr $ zip binds1 binds2 else matchCase match xs)
matchCase val ((CI DEFAULT [] caseExpr):xs) = if isWHNF val then Just caseExpr else Nothing
matchCase _ _ = Nothing

-- RULE: if the result is not equal to the last parameter or consumed from stack, reroll the stack else return the last parameter
simplifyStep :: ProveLanguage -> [ProveExpression] -> [ProveName] -> ProveExpression -> Either ProveExpression ProveExpression
--simplifyStep _ (Literal (Typed _):stack) free (Literal Evaluated)
--    = rerollStack stack $ Literal Evaluated
simplifyStep _ _ _ (Literal n) = Left (Literal n)
simplifyStep lang stack free (Variable f)
    | f `elem` free = Left (Variable f)
    | isPrimitive (Variable f) = executedVar
    | f_def /= Variable f = trace "Rule: variable_def" $ Right $ rerollStack stack f_def
    | otherwise = Left $ Variable f
    where
        executedVar = primitiveExecute stack (Variable f)
        f_def = defExpr $ getVariableDef lang stack f
simplifyStep lang (x:stack) free (Lambda n e) = 
    case simple_e of 
        Left _ -> trace "Rule: lambda simplification" $ Right $ rerollStack stack $ replaceVariable e n x
        Right s_e -> Right $ rerollStack (x:stack) $ Lambda n s_e
    where
        simple_e = simplifyStep lang [] (n:free) e
simplifyStep lang [] free (Lambda x e)
    = fmap (Lambda x) (simplifyStep lang [] (x:free) e)
simplifyStep lang stack free (Case ce b alts) =
    case matchCase ce alts of
        Nothing ->
            case case_simplified of
                Left _ -> case cases_simplified of
                    Left _ -> Left $ Case ce b alts
                    Right cs -> trace "Rule: dict_case simplification" $ Right $ rerollStack stack $ Case ce b cs
                Right e -> trace "Rule: dict_match simplification" $ Right $ rerollStack stack $ Case e b alts
        Just matched -> trace ("Rule: dict_matching: " ++ show matched) $ Right $ rerollStack stack (replaceVariable matched b ce)
    where
        cases_simplified = simplifyStepCases lang free alts
        case_simplified = simplifyStep lang [] free ce
simplifyStep lang stack free (DirectOperation dof x) =
    case f_Simplify of
        Left _ -> case x_Simplify of
            Left _ -> Left $ DirectOperation dof x
            Right x_simp -> Right $ rerollStack stack $ DirectOperation dof x_simp
        Right f_simp -> Right $ f_simp
    where
        x_Simplify = simplifyStep lang [] free x
        f_Simplify = simplifyStep lang (x:stack) free dof