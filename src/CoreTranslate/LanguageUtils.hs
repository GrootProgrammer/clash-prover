{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeApplications #-}
module CoreTranslate.LanguageUtils (
    getVariableDef,
    Node(..),
    showNode,
    tographviz,
    rerollStack,
    replaceVariable,
    isWHNF
) where

import CoreTranslate.Language
import GHC.Plugins hiding (Case)
import CoreTranslate.Translate
import Debug.Trace

getVariableDef :: ProveLanguage -> [ProveExpression] -> ProveName -> VariableDef
getVariableDef (x:xs) context n
    | defName x == n = x
    | otherwise = getVariableDef xs context n
getVariableDef [] context n = traceShow ("id: " ++ show n) $ traceShowId $ getExternalVariableDef context n

data Node = N String [(String, Node)]

writeNode :: String -> String -> String
writeNode name n = "\tnode_" ++ n ++ "[label=\"" ++ (init . drop 1) (show name) ++ "\"]\n"

writeConnection :: String -> String -> String -> String
writeConnection from to label = "\tnode_" ++ from ++ " -> " ++ "node_" ++ to ++ "[label=\"" ++ label ++ "\"]\n"


showNode :: Node -> String -> String
showNode (N name (x:xs)) path = showNode (snd x) (path ++ fst x) ++ writeConnection path (path ++ fst x) ((init . drop 1) $ show $ fst x) ++ showNode (N name xs) path
showNode (N name []) path = writeNode name path

tographvizCI :: AltCon -> [ProveName] -> ProveExpression -> Node
tographvizCI con pn pe = N ("case: " ++ showPprUnsafe con) (("expression", tographviz pe) : zipWith (\bind i -> ("binding_" ++ (show @Integer) i, N (show bind) [])) pn [1..])

tographviz :: ProveExpression -> Node
tographviz (Literal (Dict n alts)) = N (show n) $ zipWith (\a i -> ("elem_" ++ (show @Integer) i, tographviz a)) alts [1..] 
tographviz (Literal n) = N (show (Literal n)) []
tographviz (Variable n) = N (show (Variable n)) []
tographviz (Lambda n e) = N "lambda" [("expression", tographviz e), ("binding", tographviz (Variable n))]
tographviz (Case n b e) = N "case" (("match", tographviz n) : ("result_bind", N (show b) []) : zipWith (\(CI a p e) i -> ("case_" ++ (show @Integer) i, tographvizCI a p e)) e [1..])
tographviz (DirectOperation e a) = N "Operation" [("expression", tographviz e), ("arg", tographviz a)]

rerollStack :: [ProveExpression] -> ProveExpression -> ProveExpression
rerollStack xs e = foldl DirectOperation e xs

isWHNF :: ProveExpression -> Bool
isWHNF (Variable {}) = False
isWHNF (Literal {}) = True
isWHNF (Lambda {}) = False
isWHNF (Case {}) = False
isWHNF (DirectOperation e _) = isWHNF e
