module Language.LanguageUtils
  ( createIntCI,
    replaceVariable,
    isWHNF,
    getConstructorFromType,
    getProveNameForDatacon,
  )
where

import qualified GHC.Plugins as GP
import Language.Language

-- | create a casematch for a specific integer number, mostly used for enum types.
createIntCI :: Integer -> [ProveName] -> ProveExpression -> CaseInstance
createIntCI i = CI (CaseLit $ LNumber NLitNumInteger i)

-- | recursivly replace the first param by the third param if it matches the second param in a type, does type deconstruction and reconstruction to achieve this.
recursiveSplitReplace :: GP.Type -> ProveName -> GP.Type -> GP.Type
recursiveSplitReplace original from to = case GP.splitAppTy_maybe original of
  Just (left, right) -> GP.mkAppTy (recursiveSplitReplace left from to) (recursiveSplitReplace right from to)
  Nothing -> case GP.getTyVar_maybe original of
    Just _id -> if PN _id == from then to else original
    Nothing -> original

-- | recursivly replace the first param by the third param if it matches the second param.
-- Does not replace if encountering a lambda with the same ProveName to preserve correctness in recursive calls.
replaceVariable :: ProveExpression -> ProveName -> ProveExpression -> ProveExpression
replaceVariable (Variable n) from to
  | n == from = to
  | otherwise = Variable n
replaceVariable (Literal (Typed (PT t))) from (Literal (Typed (PT t2))) =
  Literal $
    Typed $
      PT $
        recursiveSplitReplace t from t2
replaceVariable (Literal (Constructor n expr)) from to = Literal $ Constructor n $ fmap (\e -> replaceVariable e from to) expr
replaceVariable (Literal l) _ _ = Literal l
replaceVariable (Lambda n e) from to
  | n == from = Lambda n e
  | otherwise = Lambda n (replaceVariable e from to)
replaceVariable (Case e b alts) a (Variable c) =
  Case (replaceVariable e a (Variable c)) (if a == c then b else a) (fmap (\(CI con binds ae) -> CI con binds (replaceVariable ae a (Variable c))) alts)
replaceVariable (Case e b alts) from to =
  Case (replaceVariable e from to) b (fmap (\(CI con binds ae) -> CI con binds (replaceVariable ae from to)) alts)
replaceVariable (DirectOperation dof a) from to = DirectOperation (replaceVariable dof from to) (replaceVariable a from to)
replaceVariable (Let (Def n g) e) from to
  | n == from = Let (Def n g) e
  | otherwise = Let (Def n (replaceVariable e from to)) (replaceVariable e from to)

-- | checks if the given expression is in WHNF (can be matched against).
isWHNF :: ProveExpression -> Bool
isWHNF (Variable {}) = False
isWHNF (Literal {}) = True
isWHNF (Lambda {}) = False
isWHNF (Case {}) = False
isWHNF (DirectOperation e _) = isWHNF e
isWHNF (Let _ e) = isWHNF e

-- | get all the possible constructors from a type, might fail but has not been observed yet
getConstructorFromType :: ProveType -> Maybe [ProveName]
getConstructorFromType (PT a) = fmap (fmap getProveNameForDatacon) (GP.tyConAppTyCon_maybe a >>= GP.tyConDataCons_maybe)

-- | get a constructors ProveName, should be equal for equivalent constructors in source code
getProveNameForDatacon :: GP.DataCon -> ProveName
getProveNameForDatacon = PN . GP.dataConWrapId
