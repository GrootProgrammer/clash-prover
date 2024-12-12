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

type RewriteRule a l = ProveLanguage l -> [Conditional l] -> ProveExpression l -> a

type PathRewriteRule l = RewriteRule (Maybe (PossiblePath l)) l

type SimpRewriteRule l = RewriteRule (Maybe (ProveExpression l)) l

pathRules :: (LanguageName l) => [PathRewriteRule l]
pathRules =
  [ --caseDestruct
  ]

--collectConstructors :: (LanguageName l) => [CaseInstance l] -> [(l, [l], ProveExpression l)]
--collectConstructors ((CI (Cons l) bindings expr):xs) = (l, bindings, expr) : collectConstructors xs
--collectConstructors (_:xs) = collectConstructors xs
--collectConstructors [] = []
--
--createNewExpr :: (LanguageName l) => (l, [l], ProveExpression l) -> ProveExpression l
--createNewExpr (_, binders, expr) = foldr Lambda expr vars
--  where
--    vars = binders
--
--createConsEq :: (LanguageName l) => (l, [l], ProveExpression l) -> l -> Conditional l
--createConsEq (c, binders, _) v = ConstEqual (Variable v) c binders
--
--casesToDeconstruct :: (LanguageName l) => [ProveExpression l] -> [CaseInstance l] -> l -> [(Conditional l, PossiblePath l)]
--casesToDeconstruct stack cis v = zip consEq expr
--  where
--    expr = map (Node . rerollStack stack . createNewExpr) cons
--    consEq = map (`createConsEq` v) cons
--    cons = collectConstructors cis

--caseDestruct :: (LanguageName l) => PathRewriteRule l
--caseDestruct _ _ stack (Case (Literal (Symbolic (Unevaluated n))) b t e) = Just $ Link
--        ((LitEqual (Variable n) (Bottom "bottom"), Node (Literal $ Bottom "bottom eval")) :
--          casesToDeconstruct stack e n)
--caseDestruct _ _ _ _ = Nothing

applyPathRules :: (LanguageName l) => PathRewriteRule l
applyPathRules lang cond = itUntilJust (map (\rule -> rule lang cond) pathRules)


tupleMaybeToMaybeTuple :: (Maybe a, b) -> Maybe (a, b)
tupleMaybeToMaybeTuple (Nothing, _) = Nothing
tupleMaybeToMaybeTuple (Just av, bv) = Just (av, bv)

deconstructStep :: (LanguageName l) => ProveLanguage l -> [Conditional l] -> PossiblePath l -> Maybe (PossiblePath l)
deconstructStep lang cond (Node expr) = case Node <$> applySimpRules lang cond expr of
  Just b -> Just b
  Nothing -> applyPathRules lang cond expr
deconstructStep lang cond (Link ((new_cond, expr) : xs)) = case deconstructStep lang (new_cond : cond) expr of
  Nothing -> case deconstructStep lang cond (Link xs) of
    Nothing -> Nothing
    Just (Link e) -> Just $ Link $ (new_cond, expr) : e
    Just (Node _) -> error "should be impossible for deconstruct Link xs to return Node"
  Just b -> Just $ Link ((new_cond, b) : xs)
deconstructStep _ _ (Link []) = Nothing
