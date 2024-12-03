module Language.LanguageUtils
  ( createIntCI,
    alphaConversion,
    typeAlphaConversion,
    isWHNF,
    getConstructorFromType,
    getProveNameForDatacon,
    getConstructorsFromName,
    getWHNFcase,
  )
where

import qualified GHC.Plugins as GP
import Language.Types

-- | create a casematch for a specific integer number, mostly used for enum types.
createIntCI :: (LanguageName l) => Integer -> [l] -> ProveExpression l -> CaseInstance l
createIntCI i = CI (CaseLit $ LNumber NLitNumInteger i)

splitType :: ProveType -> Maybe (ProveType, ProveType)
splitType (PT t) = (\(a, b) -> (PT a, PT b)) <$> GP.splitAppTy_maybe t

-- | recursivly replace the first param by the third param if it matches the second param in a type, does type deconstruction and reconstruction to achieve this.
typeAlphaConversion :: (LanguageName l) => ProveType -> l -> ProveType -> ProveType
typeAlphaConversion expr from to = case splitType expr of
  Just (left, right) -> PT $ GP.mkAppTy (_getKind $ typeAlphaConversion left from to) (_getKind (typeAlphaConversion right from to))
  Nothing -> case GP.getTyVar_maybe $ _getKind expr of
    Just _id -> if convert (PN _id) == from then to else expr
    Nothing -> expr

-- | recursivly replace the first param by the third param if it matches the second param.
-- Does not replace if encountering a lambda with the same ProveName to preserve correctness in recursive calls.
alphaConversion :: (LanguageName nType) => ProveExpression nType -> nType -> ProveExpression nType -> ProveExpression nType
alphaConversion (Variable n) from to
  | n == from = to
  | otherwise = Variable n
alphaConversion (Literal (Typed t)) from (Literal (Typed t2)) =
  Literal $
    Typed $
      typeAlphaConversion t from t2
alphaConversion (Literal (Constructor n)) _ _ = Literal (Constructor n)
alphaConversion (Literal l) _ _ = Literal l
alphaConversion (Lambda n e) from to
  | n == from = Lambda n e
  | otherwise = Lambda n (alphaConversion e from to)
alphaConversion (Case e b alts) a (Variable c) =
  Case (alphaConversion e a (Variable c)) (if a == c then b else a) (fmap (\(CI con binds ae) -> CI con binds (alphaConversion ae a (Variable c))) alts)
alphaConversion (Case e b alts) from to =
  Case (alphaConversion e from to) b (fmap (\(CI con binds ae) -> CI con binds (alphaConversion ae from to)) alts)
alphaConversion (DirectOperation dof a) from to = DirectOperation (alphaConversion dof from to) (alphaConversion a from to)
alphaConversion (Let (Def n g) e) from to
  | n == from = Let (Def n g) e
  | otherwise = Let (Def n (alphaConversion e from to)) (alphaConversion e from to)

-- | checks if the given expression is in WHNF (can be matched against).
isWHNF :: (LanguageName w) => ProveExpression w -> Bool
isWHNF (Variable {}) = False
isWHNF (Literal {}) = True
isWHNF (Lambda {}) = False
isWHNF (Case {}) = False
isWHNF (DirectOperation e _) = isWHNF e
isWHNF (Let _ e) = isWHNF e

getWHNFcase :: (LanguageName w) => ProveExpression w -> Maybe (LiteralTypes w,ProveExpression w -> ProveExpression w)
getWHNFcase (Variable {}) = Nothing
-- special case to handle constructors with data types, should find this out dynamically
-- getWHNFcase (DirectOperation (Literal l) a) = Just (l, id)
getWHNFcase (Literal l) = Just (l, id)
getWHNFcase (Lambda v e) = (\(l, f) -> (l, Lambda v . f)) <$> getWHNFcase e
getWHNFcase (Case {}) = Nothing
getWHNFcase (DirectOperation e a) = (\(l, f) -> (l, (`DirectOperation` a) . f)) <$> getWHNFcase e
getWHNFcase (Let a e) = (\(l, f) -> (l, (Let a) . f)) <$> getWHNFcase e

getConstructorsFromName :: (LanguageName l) => l -> Maybe [l]
getConstructorsFromName n = getConstructorFromType (getType n)

-- | get all the possible constructors from a type, might fail but has not been observed yet
getConstructorFromType :: (LanguageName l) => ProveType -> Maybe [l]
getConstructorFromType (PT a) = ((convert . getProveNameForDatacon) <$>) <$> (GP.tyConAppTyCon_maybe a >>= GP.tyConDataCons_maybe)

-- | get a constructors ProveName, should be equal for equivalent constructors in source code
getProveNameForDatacon :: (LanguageName l) => GP.DataCon -> l
getProveNameForDatacon = convert . PN . GP.dataConWrapId
