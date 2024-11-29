{-# LANGUAGE TypeApplications #-}

module Language.ToGraphviz
  ( Node (..),
    writeNode,
    writeConnection,
    showNode,
    toGraphviz,
  )
where

import Language.Types

-- | represents a node in the tree for Graphviz
data Node = N String [(String, Node)]

-- | write s graphviz node, correctly escapes its characters in the label
writeNode ::
  String ->
  String ->
  String
writeNode name n =
  "\tnode_" ++ n ++ "[label=\"" ++ (init . drop 1) (show name) ++ "\"]\n"

-- | Writes a connection in Graphviz, correctly escapes its characters in the label
writeConnection ::
  String ->
  String ->
  String ->
  String
writeConnection from to label =
  "\tnode_" ++ from ++ " -> " ++ "node_" ++ to ++ "[label=\"" ++ label ++ "\"]\n"

-- | Convert a Node to a string in dot notation, prefixes all nodes with the second parameter to prevent naming conflicts
showNode ::
  Node ->
  String ->
  String
showNode (N name (x : xs)) path = showNode (snd x) (path ++ fst x) ++ writeConnection path (path ++ fst x) ((init . drop 1) $ show $ fst x) ++ showNode (N name xs) path
showNode (N name []) path = writeNode name path

-- | converts a case instance to graphviz notation
toGraphvizCI :: (LanguageName l) => CaseMatch l -> [l] -> ProveExpression l -> Node
toGraphvizCI con pn pe = N ("case: " ++ show con) (("expression", toGraphviz pe) : zipWith (\bind i -> ("binding_" ++ (show @Integer) i, N (show bind) [])) pn [1 ..])

-- | Recursivly converts a given expression into a Node meant for rendering in graphviz notation
-- Chaining it with showNode ( showNode (toGraphviz expression) "" ) gives a proper graphviz equivalent of the expression without the "digraph g { %s }" part.
toGraphviz :: (LanguageName l) => ProveExpression l -> Node
toGraphviz (Literal (Constructor n expr)) = N ("constructor: " ++ getShortName n ++ ", " ++ show (take 1 expr)) []
toGraphviz (Literal n) = N (show (Literal n)) []
toGraphviz (Variable n) = N (getShortName n) []
toGraphviz (Lambda n e) = N "lambda" [("expression", toGraphviz e), ("binding", toGraphviz $ Variable n)]
toGraphviz (Case n b e) = N "case" (("match", toGraphviz n) : ("result_bind", N (show b) []) : zipWith (\(CI a p cie) i -> ("case_" ++ (show @Integer) i, toGraphvizCI a p cie)) e [1 ..])
toGraphviz (DirectOperation e a) = N "Operation" [("expression", toGraphviz e), ("arg", toGraphviz a)]
toGraphviz (Let (Def n ne) e) = N "Let" [("expression", toGraphviz e), ("binding", toGraphviz $ Variable n), ("let_expression", toGraphviz ne)]
