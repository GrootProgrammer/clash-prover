module Utils.ToGraphviz
  ( GraphvizNode (..),
    DotAble (..),
    writeNode,
    writeConnection,
    showNode,
    showTNode,
  )
where

-- | represents a node in the tree for Graphviz
data GraphvizNode = Node String [(String, GraphvizNode)]

isPrintable :: Char -> Bool
isPrintable c = (c `elem` ['a' .. 'z']) || (c `elem` ['A' .. 'Z']) || (c `elem` ['0' .. '9']) || (c `elem` [' ', ':', '(', ')', '[', ']', '.', '"', '\\'])

getAllowedChars :: String -> String
getAllowedChars = filter isPrintable

-- | write s graphviz node, correctly escapes its characters in the label
writeNode ::
  String ->
  GraphvizNode ->
  String
writeNode path (Node name _) =
  "\t" ++ getNodeBinding path ++ "[label=\"" ++ name ++ "\"]\n"

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
  "\t" ++ getNodeBinding from ++ " -> " ++ getNodeBinding to ++ "[label=\"" ++ label ++ "\"]\n"

-- | Convert a Node to a string in dot notation, prefixes all nodes with the second parameter to prevent naming conflicts
showNode ::
  String ->
  GraphvizNode ->
  String
showNode currPath (Node name ((path, child) : xs)) = showNode (currPath ++ "_" ++ path) child ++ writeConnection currPath (currPath ++ "_" ++ path) path ++ showNode currPath (Node name xs)
showNode currPath (Node name []) = writeNode currPath (Node name [])

class DotAble a where
  toNode :: a -> GraphvizNode

instance DotAble GraphvizNode where
  toNode = id

showTNode :: (Show a) => a -> GraphvizNode
showTNode v = Node (show v) []
