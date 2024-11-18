module Language.LanguageUtils (
  createIntCI,
  replaceVariable,
  isWHNF,
  getConstructorFromType,
  getProveNameForDatacon
) where

import qualified GHC.Plugins as GP

import Language.Language

createIntCI :: Integer -> [ProveName] -> ProveExpression -> CaseInstance
createIntCI i = CI (CaseLit $ LNumber NLitNumInteger i)

replaceVariable :: ProveExpression -> ProveName -> ProveExpression -> ProveExpression
replaceVariable (Variable n) from to
  | n == from = to
  | otherwise = Variable n
replaceVariable (Literal (Typed (PT t))) from (Literal (Typed (PT t2))) =
  Literal
  $ Typed 
  $ PT 
  $ case GP.getTyVar_maybe t of
    Just _id -> if PN _id == from then t2 else t
    Nothing -> t
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

isWHNF :: ProveExpression -> Bool
isWHNF (Variable {}) = False
isWHNF (Literal {}) = True
isWHNF (Lambda {}) = False
isWHNF (Case {}) = False
isWHNF (DirectOperation e _) = isWHNF e
isWHNF (Let _ e) = isWHNF e

getConstructorFromType :: ProveType -> Maybe [ProveName]
getConstructorFromType (PT a) =  fmap (fmap getProveNameForDatacon) (GP.tyConAppTyCon_maybe a >>= GP.tyConDataCons_maybe)

getProveNameForDatacon :: GP.DataCon -> ProveName
getProveNameForDatacon = PN . GP.dataConWrapId
