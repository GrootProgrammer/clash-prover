module Rewrite.Primitives where

import Data.Foldable (find)
import Debug.Trace (trace)
import GHC.Data.Maybe (firstJusts)
import Language.Expression (ExprRep (..), TypeRep (TypeConstructor), VarRep (varName), varUniquePrim)

type PrimRule = ExprRep -> Maybe ExprRep

type PrimStackUse = ExprRep -> Maybe Integer

primitivesInfo :: [(String, Integer, PrimRule)]
primitivesInfo =
  [ ("$ghc-prim$GHC.Prim$dataToTag#", 2, primDataToTag),
    ("$ghc-bignum$GHC.Num.Natural$naturalGe", 2, notImplemented)
  ]

isPrimitive :: VarRep -> Bool
isPrimitive s = (varUniquePrim s) `elem` (map (\(m, _, _) -> m) primitivesInfo)

getPrimStackUsage :: ExprRep -> Maybe Integer
getPrimStackUsage (ExprApp e _) = do
  stack <- getPrimStackUsage e
  (if stack > 1 then Just $ stack - 1 else Nothing)
getPrimStackUsage (ExprVar v) = trace ("checking primitive: " ++ varUniquePrim v) $ (\(_, i, _) -> i) <$> find (\(s, _, _) -> varUniquePrim v == s) primitivesInfo
getPrimStackUsage _ = Nothing

applyPrimRules :: PrimRule
applyPrimRules expr = firstJusts $ map (\(_, _, r) -> r expr) primitivesInfo

primDataToTag :: PrimRule
primDataToTag (ExprApp (ExprApp (ExprVar v) (ExprType (TypeConstructor (arg_name, applied, consts) args))) (ExprVar v2))
  | varUniquePrim v == "$ghc-prim$GHC.Prim$dataToTag#" =
      Nothing
  | otherwise = Nothing
primDataToTag _ = Nothing

notImplemented :: PrimRule
notImplemented _ = Nothing
