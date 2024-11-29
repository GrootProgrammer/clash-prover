{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rewrite.Deconstruct where

import Data.Maybe
import Data.Maybe (Maybe (Nothing))
import Language
import Prelude

data (LanguageName l) => Conditional l
  = LitEqual (ProveExpression l) (LiteralTypes l)
  | ConstEqual (ProveExpression l) l [l]
  | OpResult l (ProveExpression l) [l]
  | Not [Conditional l]
  deriving (Show)

data (LanguageName l) => PossiblePath l
  = Node (ProveExpression l)
  | Link [(Conditional l, PossiblePath l)]
  deriving (Show)

toGraphvizPath :: (LanguageName l) => PossiblePath l -> Node
toGraphvizPath (Node expr) = toGraphviz expr
toGraphvizPath (Link xs) = N "caseSplit" (map (\(cond, path) -> (show cond, toGraphvizPath path)) xs)

type RewriteRule a l = ProveLanguage l -> [ProveName] -> [Conditional l] -> ProveExpression l -> a

type PathRewriteRule l = RewriteRule (Maybe [PossiblePath l]) l

type SimpRewriteRule l = RewriteRule (Maybe (ProveExpression l)) l

pathRules :: [PathRewriteRule l]
pathRules = []

applyPathRules :: (LanguageName l) => PathRewriteRule l
applyPathRules lang free cond = itUntilJust (map (\rule -> rule lang free cond) pathRules)

simplifyRules :: (LanguageName l) => [SimpRewriteRule l]
simplifyRules =
  [ betaReduction
  ]

betaReduction :: (LanguageName l) => SimpRewriteRule l
betaReduction _ _ _ (DirectOperation (Lambda n e) arg) = Just $ alphaConversion e n arg
betaReduction _ _ _ _ = Nothing

applySimpRules :: (LanguageName l) => SimpRewriteRule l
applySimpRules lang free cond = itUntilJust (map (\rule -> rule lang free cond) simplifyRules)

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
deconstructStep lang cond (Node expr) = Node <$> applySimpRules lang [] cond expr
deconstructStep lang cond (Link ((new_cond, expr) : xs)) = case deconstructStep lang (new_cond : cond) expr of
  Nothing -> case deconstructStep lang cond (Link xs) of
    Nothing -> Nothing
    Just (Link e) -> Just $ Link $ (new_cond, expr) : e
    Just (Node _) -> error "should be impossible for deconstruct Link xs to return Node"
  Just b -> Just $ Link ((new_cond, b) : xs)
deconstructStep _ _ (Link []) = Nothing
