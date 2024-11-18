{-# LANGUAGE TypeApplications #-}

module Language.ToGraphviz (
  Node(..),
  writeNode,
  writeConnection,
  showNode,
  toGraphviz,
) where

import Language.Language

data Node = N String [(String, Node)]

writeNode
  :: String
  -> String
  -> String
writeNode name n =
  "\tnode_" ++ n ++ "[label=\"" ++ (init . drop 1) (show name) ++ "\"]\n"

writeConnection
  :: String
  -> String
  -> String
  -> String
writeConnection from to label =
  "\tnode_" ++ from ++ " -> " ++ "node_" ++ to ++ "[label=\"" ++ label ++ "\"]\n"

showNode
  :: Node
  -> String
  -> String
showNode (N name (x:xs)) path = showNode (snd x) (path ++ fst x) ++ writeConnection path (path ++ fst x) ((init . drop 1) $ show $ fst x) ++ showNode (N name xs) path
showNode (N name []) path = writeNode name path

toGraphvizCI :: CaseMatch -> [ProveName] -> ProveExpression -> Node
toGraphvizCI con pn pe = N ("case: " ++ show con) (("expression", toGraphviz pe) : zipWith (\bind i -> ("binding_" ++ (show @Integer) i, N (show bind) [])) pn [1..])

toGraphviz :: ProveExpression -> Node
toGraphviz (Literal n) = N (show (Literal n)) []
toGraphviz (Variable n) = N (show (Variable n)) []
toGraphviz (Lambda n e) = N "lambda" [("expression", toGraphviz e), ("binding", toGraphviz $ Variable n)]
toGraphviz (Case n b e) = N "case" (("match", toGraphviz n) : ("result_bind", N (show b) []) : zipWith (\(CI a p cie) i -> ("case_" ++ (show @Integer) i, toGraphvizCI a p cie)) e [1..])
toGraphviz (DirectOperation e a) = N "Operation" [("expression", toGraphviz e), ("arg", toGraphviz a)]
toGraphviz (Let (Def n ne) e) = N "Let" [("expression", toGraphviz e), ("binding", toGraphviz $ Variable n), ("let_expression", toGraphviz ne)]
