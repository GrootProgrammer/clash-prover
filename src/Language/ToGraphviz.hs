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

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

-- path, label, child
type ConnectionInfo = (String, String, Node)

-- | represents a node in the tree for Graphviz
data Node = N String [ConnectionInfo]

-- | write s graphviz node, correctly escapes its characters in the label
writeNode ::
  String ->
  Node ->
  String
writeNode path (N name _) =
  "\t" ++ getNodeBinding path ++ "[label=\"" ++ (init . drop 1) (show name) ++ "\"]\n"

getNodeBinding ::
  String ->
  String
getNodeBinding path = "node_" ++ path

-- | Writes a connection in Graphviz, correctly escapes its characters in the label
writeConnection ::
  String ->
  String ->
  String ->
  String
writeConnection from to label =
  "\t" ++ getNodeBinding from ++ " -> " ++ getNodeBinding to ++ "[label=\"" ++ replace '"' ' ' label ++ "\"]\n"

-- | Convert a Node to a string in dot notation, prefixes all nodes with the second parameter to prevent naming conflicts
showNode ::
  String ->
  Node ->
  String
showNode currPath (N name ((path, label, child) : xs)) = showNode (currPath ++ path) child ++ writeConnection currPath (currPath ++ path) label ++ showNode currPath (N name xs)
showNode currPath (N name []) = writeNode currPath (N name [])

-- | converts a case instance to graphviz notation
toGraphvizCI :: (LanguageName l) => CaseMatch l -> [l] -> ProveExpression l -> Node
toGraphvizCI con pn pe = N ("case: " ++ show con) (("expression", "expression", toGraphviz pe) : zipWith (\bind i -> ("binding_" ++ (show @Integer) i, "binding_" ++ (show @Integer) i, N (show bind) [])) pn [1 ..])

-- | Recursivly converts a given expression into a Node meant for rendering in graphviz notation
-- Chaining it with showNode ( showNode (toGraphviz expression) "" ) gives a proper graphviz equivalent of the expression without the "digraph g { %s }" part.
toGraphviz :: (LanguageName l) => ProveExpression l -> Node
toGraphviz (Literal (Constructor n)) = N ("constructor: " ++ getShortName n ) []
toGraphviz (Literal n) = N (show (Literal n)) []
toGraphviz (Variable n) = N (getName n) []
toGraphviz (Lambda n e) = N "lambda" [("expression", "expression", toGraphviz e), ("binding", "binding", toGraphviz $ Variable n)]
toGraphviz (Case n b e) = N "case" (("match", "match", toGraphviz n) : ("result_bind", "result_bind", N (show b) []) : zipWith (\(CI a p cie) i -> ("case_" ++ (show @Integer) i, "case_" ++ show i, toGraphvizCI a p cie)) e [1 ..])
toGraphviz (DirectOperation e a) = N "Operation" [("expression", "expression", toGraphviz e), ("arg", "arg", toGraphviz a)]
toGraphviz (Let (Def n ne) e) = N "Let" [("expression", "expression", toGraphviz e), ("binding", "binding", toGraphviz $ Variable n), ("let_expression", "let_expr", toGraphviz ne)]
