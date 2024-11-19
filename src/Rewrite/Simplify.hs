{-# LANGUAGE BlockArguments #-}

module Rewrite.Simplify where

import Data.Foldable (Foldable (foldl'))
import Debug.Trace
import Language
import Primitives

-- the type of a function that implements a rewrite rule, should return Left input of not executed and (fmap (rerollStack stack) Right transformed) if executed
type RewriteRule = ProveLanguage -> [ProveExpression] -> [ProveName] -> ProveExpression -> Either ProveExpression ProveExpression

rules :: [RewriteRule]
rules =
  [ primitiveApply,
    caseMatchCons,
    --    unfoldVar,
    lambdaApply
  ]

primitiveApply :: RewriteRule
primitiveApply _ stack _ (Variable op) = (Right . trace "applied rule: primitive OP") =<< primitiveExecute stack (Variable op)
primitiveApply _ _ _ expr = Left expr

unfoldVar :: RewriteRule
unfoldVar lang stack free (Variable var)
  | var `elem` free = Left (Variable var)
  | Primitives.isPrimitive (Variable var) = Left (Variable var)
  | otherwise = if Language.isPrimitive unfolded then Left (Variable var) else trace "applied rule: unfoldVar" $ Right $ rerollStack stack $ defExpr unfolded
  where
    unfolded = getVariableDef lang stack var
unfoldVar _ _ _ expr = Left expr

lambdaApply :: RewriteRule
lambdaApply _ (x : stack) _ (Lambda n e) = trace "applied rule: Lambda reduction" $ rerollStack stack <$> Right (replaceVariable e n x)
lambdaApply _ _ _ expr = Left expr

getMatchingCons :: [CaseInstance] -> ProveName -> Maybe CaseInstance
getMatchingCons ((CI DEFAULT binders arg) : xs) constructor = case getMatchingCons xs constructor of
  Just c -> Just c
  Nothing -> Just (CI DEFAULT binders arg)
getMatchingCons ((CI (Cons c) binders arg) : xs) constructor = if c == constructor then Just $ CI (Cons c) binders arg else getMatchingCons xs constructor
getMatchingCons _ _ = Nothing

caseMatchCons :: RewriteRule
caseMatchCons lang stack free (Case (Literal (Constructor name consBindings)) binding options) = case getMatchingCons options name of
  Just (CI _ bindersMatch argMatch) ->
    trace "applied rule: caseMatchCons" $
      Right $
        rerollStack stack $
          foldl'
            (\expr (from, to) -> replaceVariable expr from to)
            (replaceVariable argMatch binding (Literal (Constructor name consBindings)))
            (zip bindersMatch consBindings)
  Nothing -> Left (Case (Literal (Constructor name consBindings)) binding options)
caseMatchCons _ _ _ expr = Left expr

iterateOverFuncListUntilRight :: [a -> Either a a] -> a -> Either a a
iterateOverFuncListUntilRight (x : xs) input = case x input of
  Left _ -> iterateOverFuncListUntilRight xs input
  Right out -> Right out
iterateOverFuncListUntilRight [] input = Left input

applyRules :: ProveLanguage -> [ProveExpression] -> [ProveName] -> ProveExpression -> Either ProveExpression ProveExpression
applyRules lang stack free = iterateOverFuncListUntilRight (fmap (\rule -> rule lang stack free) rules)

iterateUntilLeft :: (a -> Either a a) -> a -> a
iterateUntilLeft funcParam start =
  case funcParam start of
    Left _ -> start
    Right e -> iterateUntilLeft funcParam e

simplify :: ProveLanguage -> [ProveExpression] -> ProveExpression -> ProveExpression
simplify lang stack = iterateUntilLeft (simplifyStep lang stack [])

simplifyStepCase :: ProveLanguage -> [ProveName] -> [CaseInstance] -> Either [CaseInstance] [CaseInstance]
simplifyStepCase _ _ [] = Left []
simplifyStepCase lang free ((CI match names expression) : xs) = case simplifyStep lang [] (names ++ free) expression of
  Left _ -> either (Left . (CI match names expression :)) (Right . (CI match names expression :)) $ simplifyStepCase lang free xs
  Right result -> Right $ CI match names result : xs

simplifyStep :: RewriteRule
simplifyStep lang stack free expr =
  case applied of
    Left _ -> case expr of
      (Literal l) -> Left $ Literal l
      (Variable n) -> Left $ Variable n
      (Lambda n e) -> either (\_ -> Left $ Lambda n e) (Right . Lambda n) $ simplifyStep lang [] (n : free) e
      (Let (Def n ne) e) ->
        either
          (\r -> Left $ Let (Def n r) e)
          (\r -> Right $ rerollStack stack $ Let (Def n r) e)
          $ applyRules lang [] (n : free) ne
      (Case match bindings options) -> case simplifyStep lang [] free match of
        Left _ -> either (\_ -> Left $ Case match bindings options) (Right . Case match bindings) $ simplifyStepCase lang (bindings : free) options
        Right result -> Right $ rerollStack stack $ Case result bindings options
      (DirectOperation f a) -> case simplifyStep lang (a : stack) free f of
        Left _ -> case simplifyStep lang [] free a of
          Left _ -> Left $ DirectOperation f a
          Right result -> Right $ rerollStack stack $ DirectOperation f result
        Right result -> Right result
    Right result -> Right result
  where
    applied = applyRules lang stack free expr
