{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Haskell2010 #-}

module Rewrite.Deconstruct where

import Data.Maybe
import Language
import Prelude
import Primitives

data (LanguageName l) => Conditional l
  = LitEqual (ProveExpression l) (LiteralTypes l)
  | ConstEqual (ProveExpression l) l [l]
  | Not [Conditional l]

instance (LanguageName l) => Show (Conditional l) where
  show (LitEqual expr l) = show expr ++ " == " ++ show l
  show (ConstEqual expr l arg) = show expr ++ " == " ++ show l ++ show (map show arg)
  show (Not conds) = "not " ++ show conds

data (LanguageName l) => PossiblePath l
  = Node (ProveExpression l)
  | Link [(Conditional l, PossiblePath l)]
  deriving (Show)

toGraphvizPath :: (LanguageName l) => PossiblePath l -> Node
toGraphvizPath (Node expr) = toGraphviz expr
toGraphvizPath (Link xs) = N "input" (zipWith (\(cond, path) n -> (show n, show cond, toGraphvizPath path)) xs [1 ..])

type RewriteRule a l = ProveLanguage l -> [l] -> [Conditional l] -> [ProveExpression l] -> ProveExpression l -> a

type PathRewriteRule l = RewriteRule (Maybe (PossiblePath l)) l

type SimpRewriteRule l = RewriteRule (Maybe (ProveExpression l)) l

pathRules :: (LanguageName l) => [PathRewriteRule l]
pathRules =
  [ caseDestruct
  ]

collectConstructors :: (LanguageName l) => [CaseInstance l] -> [(l, [l], ProveExpression l)]
collectConstructors ((CI (Cons l) bindings expr):xs) = (l, bindings, expr) : collectConstructors xs
collectConstructors (_:xs) = collectConstructors xs
collectConstructors [] = []

createNewExpr :: (LanguageName l) => (l, [l], ProveExpression l) -> ProveExpression l
createNewExpr (_, binders, expr) = foldr Lambda expr vars
  where
    vars = binders

createConsEq :: (LanguageName l) => (l, [l], ProveExpression l) -> l -> Conditional l
createConsEq (c, binders, _) v = ConstEqual (Variable v) c binders

casesToDeconstruct :: (LanguageName l) => [ProveExpression l] -> [CaseInstance l] -> l -> [(Conditional l, PossiblePath l)]
casesToDeconstruct stack cis v = zip consEq expr
  where
    expr = map (Node . rerollStack stack . createNewExpr) cons
    consEq = map (`createConsEq` v) cons
    cons = collectConstructors cis

caseDestruct :: (LanguageName l) => PathRewriteRule l
caseDestruct _ _ _ stack (Lambda n1 (Case (Variable n2) b e))
  | n1 == n2 = Just $ Link
        ((LitEqual (Variable n1) (Bottom "bottom"), Node (Literal $ Bottom "bottom eval")) :
          casesToDeconstruct stack e n1)
  | otherwise = Nothing
caseDestruct _ _ _ _ _ = Nothing

applyPathRules :: (LanguageName l) => PathRewriteRule l
applyPathRules lang free cond stack = itUntilJust (map (\rule -> rule lang free cond stack) pathRules)

simplifyRules :: (LanguageName l) => [SimpRewriteRule l]
simplifyRules =
  [ alphaConversionRule,
    unfoldVar,
    trivialCase,
    primitiveEvalStack,
    primitiveEval,
    lookInsideLambda,
    lookInsideDirect,
    lookInsideCase
  ]

alphaConversionRule :: (LanguageName l) => SimpRewriteRule l
alphaConversionRule _ _ _ (x:xs) (Lambda e v) = Just $ rerollStack xs $ alphaConversion v e x
alphaConversionRule _ _ _ _ _ = Nothing

unfoldVar :: (LanguageName l) => SimpRewriteRule l
unfoldVar lang free _ stack (Variable var)
  | var `elem` free = Nothing
  | Primitives.isPrimitive (Variable var) = Nothing
  | otherwise = if Language.isPrimitive unfolded then Nothing else Just $ rerollStack stack $ defExpr unfolded
  where
    unfolded = getVariableDef lang stack var
unfoldVar _ _ _ _ _ = Nothing


applySimpRulesIt :: (LanguageName l) => ProveLanguage l -> [l] -> [Conditional l] -> [ProveExpression l] -> Maybe [ProveExpression l]
applySimpRulesIt lang free cond (expr:xs) = case applySimpRules lang free cond [] expr of
    Just simp -> Just (simp : xs)
    Nothing -> (expr :) <$> applySimpRulesIt lang free cond xs
applySimpRulesIt _ _ _ [] = Nothing

primitiveEval :: (LanguageName l) => SimpRewriteRule l
primitiveEval lang free cond stack (Variable var)
  | var `elem` free = Nothing
  | Primitives.isPrimitive (Variable var) = case primitiveExecute stack (Variable var) of
      Left _ -> Nothing
      Right simp -> Just simp
  | otherwise = if Language.isPrimitive unfolded then Nothing else Just $ rerollStack stack $ defExpr unfolded
  where
    unfolded = getVariableDef lang stack var
primitiveEval _ _ _ _ _ = Nothing

primitiveEvalStack :: (LanguageName l) => SimpRewriteRule l
primitiveEvalStack lang free cond stack (Variable var) =
    if toInteger (length stack) < stack_length then
        Nothing
      else
        case applySimpRulesIt lang free cond $ take (fromInteger stack_length) stack of
          Nothing -> Nothing
          Just simp -> Just $ rerollStack (drop (fromInteger stack_length) stack) $ rerollStack simp (Variable var)
  where
    stack_length = primitiveConsumption (Variable var)
primitiveEvalStack _ _ _ _ _ = Nothing

getMatchingCons :: (LanguageName l) => [CaseInstance l] -> LiteralTypes l -> Maybe (CaseInstance l)
getMatchingCons ((CI DEFAULT binders arg) : xs) constructor = case getMatchingCons xs constructor of
  Just c -> Just c
  Nothing -> Just (CI DEFAULT binders arg)
getMatchingCons ((CI (Cons c) binders arg) : xs) (Constructor constructor) = if c == constructor then Just $ CI (Cons c) binders arg else getMatchingCons xs (Constructor constructor)
getMatchingCons _ _ = Nothing

trivialCase :: (LanguageName l) => SimpRewriteRule l
trivialCase _ _ _ stack (Case m _ e) = case getWHNFcase m of
  Nothing -> Nothing
  Just (c,expr_f) -> case getMatchingCons e c of
    Nothing -> Nothing
    Just (CI _ binders arg) -> Just $ rerollStack stack $ expr_f $ foldr Lambda arg binders
trivialCase _ _ _ _ _ = Nothing

lookInsideLambda :: (LanguageName l) => SimpRewriteRule l
lookInsideLambda lang free cond [] (Lambda v e) = Lambda v <$> applySimpRules lang (v:free) cond [] e
lookInsideLambda _ _ _ _ _ = Nothing

lookInsideDirect :: (LanguageName l) => SimpRewriteRule l
lookInsideDirect lang free cond stack (DirectOperation e arg) = applySimpRules lang free cond (arg : stack) e
lookInsideDirect _ _ _ _ _ = Nothing

lookInsideCase :: (LanguageName l) => SimpRewriteRule l
lookInsideCase lang free cond stack (Case m b e) = rerollStack stack . (\r -> Case r b e) <$> applySimpRules lang free cond [] m
lookInsideCase _ _ _ _ _ = Nothing

applySimpRules :: (LanguageName l) => SimpRewriteRule l
applySimpRules lang free cond stack = itUntilJust (map (\rule -> rule lang free cond stack) simplifyRules)

itUntilJust :: forall a b. [a -> Maybe b] -> a -> Maybe b
itUntilJust funcList v = listToMaybe $ mapMaybe ($ v) funcList

iterateUntilNothing :: (a -> Maybe a) -> a -> a
iterateUntilNothing funcParam start = case funcParam start of
  Nothing -> start
  Just b -> iterateUntilNothing funcParam b

tupleMaybeToMaybeTuple :: (Maybe a, b) -> Maybe (a, b)
tupleMaybeToMaybeTuple (Nothing, _) = Nothing
tupleMaybeToMaybeTuple (Just av, bv) = Just (av, bv)

deconstructStep :: (LanguageName l) => ProveLanguage l -> [Conditional l] -> PossiblePath l -> Maybe (PossiblePath l)
deconstructStep lang cond (Node expr) = case Node <$> applySimpRules lang [] cond [] expr of
  Just b -> Just b
  Nothing -> applyPathRules lang [] cond [] expr
deconstructStep lang cond (Link ((new_cond, expr) : xs)) = case deconstructStep lang (new_cond : cond) expr of
  Nothing -> case deconstructStep lang cond (Link xs) of
    Nothing -> Nothing
    Just (Link e) -> Just $ Link $ (new_cond, expr) : e
    Just (Node _) -> error "should be impossible for deconstruct Link xs to return Node"
  Just b -> Just $ Link ((new_cond, b) : xs)
deconstructStep _ _ (Link []) = Nothing
