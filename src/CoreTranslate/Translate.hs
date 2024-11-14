module CoreTranslate.Translate (convertBinds, convertExpression, replaceVariable, getExternalVariableDef) where

import CoreTranslate.Language
import GHC.Plugins
import Prelude

convertBinds :: [CoreBind] -> ProveLanguage
convertBinds = concatMap convertBind

convertBind :: CoreBind -> ProveLanguage
convertBind (NonRec _id expr) = [convertIdExpression _id expr]
convertBind (Rec l) = concatMap (\(_id, expr) -> convertBind (NonRec _id expr)) l

convertIdExpression :: CoreBndr -> CoreExpr -> VariableDef
convertIdExpression name expr = Def (PN name) (convertExpression expr)

replaceVariable :: ProveExpression -> ProveName -> ProveExpression -> ProveExpression
replaceVariable (Variable n) from to
  | n == from = to
  | otherwise = Variable n
replaceVariable (Literal (Dict d alts)) from to =
  Literal (Dict d (fmap (\e -> replaceVariable e from to) alts))
replaceVariable (Literal (Typed (PT t))) from (Literal (Typed (PT t2))) =
  case getTyVar_maybe t of
    Just id -> if (PN id) == from then (Literal $ Typed $ PT t2) else (Literal $ Typed $ PT t)
    Nothing -> (Literal $ Typed $ PT t)
  
replaceVariable (Literal l) _ _ = Literal l
replaceVariable (Lambda n e) from to
  | n == from = Lambda n e
  | otherwise = Lambda n (replaceVariable e from to)
replaceVariable (CoreTranslate.Language.Case e b alts) a (Variable c) =
  CoreTranslate.Language.Case (replaceVariable e a (Variable c)) (if a == c then b else a) (fmap (\(CI con binds ae) -> CI con binds (replaceVariable ae a (Variable c))) alts)
replaceVariable (CoreTranslate.Language.Case e b alts) from to =
  CoreTranslate.Language.Case (replaceVariable e from to) b (fmap (\(CI con binds ae) -> CI con binds (replaceVariable ae from to)) alts)
replaceVariable (DirectOperation dof a) from to = DirectOperation (replaceVariable dof from to) (replaceVariable a from to)

convertExpression :: CoreExpr -> ProveExpression
convertExpression (Var info) = Variable (PN info)
convertExpression (Lit lit) = convertLiteral lit
convertExpression (App expr arg) = DirectOperation (convertExpression expr) (convertExpression arg)
convertExpression (Lam varToBind expression) = Lambda (PN varToBind) (convertExpression expression)
convertExpression (Let (NonRec _id expr) expression) = replaceVariable (convertExpression expression) (PN _id) (convertExpression expr)
convertExpression (GHC.Plugins.Case a b _ d) = CoreTranslate.Language.Case (convertExpression a) (PN b) (fmap convertAlt d)
convertExpression (Cast e _) = convertExpression e
convertExpression (Tick _ expr) = convertExpression expr
convertExpression (Coercion _) = error "called convertExpression with Coercion"
convertExpression (Type t) = Literal $ Typed $ PT t
convertExpression t = error $ "non exhaustive: " ++ showPprUnsafe t

convertAlt :: Alt CoreBndr -> CaseInstance
convertAlt (Alt con bind expr) = CI con (fmap PN bind) (convertExpression expr)

convertNumType :: LitNumType -> NumTypes
convertNumType GHC.Plugins.LitNumBigNat = CoreTranslate.Language.LitNumBigNat
convertNumType GHC.Plugins.LitNumInt = CoreTranslate.Language.LitNumInt
convertNumType GHC.Plugins.LitNumInt8 = CoreTranslate.Language.LitNumInt8
convertNumType GHC.Plugins.LitNumInt16 = CoreTranslate.Language.LitNumInt16
convertNumType GHC.Plugins.LitNumInt32 = CoreTranslate.Language.LitNumInt32
convertNumType GHC.Plugins.LitNumInt64 = CoreTranslate.Language.LitNumInt64
convertNumType GHC.Plugins.LitNumWord = CoreTranslate.Language.LitNumWord
convertNumType GHC.Plugins.LitNumWord8 = CoreTranslate.Language.LitNumWord8
convertNumType GHC.Plugins.LitNumWord16 = CoreTranslate.Language.LitNumWord16
convertNumType GHC.Plugins.LitNumWord32 = CoreTranslate.Language.LitNumWord32
convertNumType GHC.Plugins.LitNumWord64 = CoreTranslate.Language.LitNumWord64

convertLiteral :: Literal -> ProveExpression
convertLiteral (LitChar c) =
  Literal $ LChar c
convertLiteral (LitNumber numType v) =
  Literal $ LNumber (convertNumType numType) v
convertLiteral (LitString s) =
  Literal $ LString $ show s
convertLiteral (LitFloat r) =
  Literal $ LFloat r
convertLiteral (LitDouble d) =
  Literal $ LDouble d
convertLiteral _ = error "not supported literal"

-- panics if ProveName is in the current Module
getExternalVariableDef :: [ProveExpression] -> ProveName -> VariableDef
getExternalVariableDef _ (PN _id) = case unfolding of
  NoUnfolding -> Def (PN _id) (Variable (PN _id))
  BootUnfolding -> Def (PN _id) (Variable (PN _id))
  (OtherCon []) -> Def (PN _id) (Literal (Constructor (PN _id)))
  (OtherCon [DataAlt l]) -> Def (PN _id) (Literal (Dict l [Variable (PN _id)]))
  (OtherCon xs) -> error ("constructor with multiple options: " ++ showPprUnsafe (OtherCon xs))
  (DFunUnfolding [] c args) -> Def (PN _id) (Literal (Dict c (fmap convertExpression args)))
  (DFunUnfolding vars c args) -> Def (PN _id) (foldr (Lambda . PN) (Literal (Dict c (fmap convertExpression args))) vars)
  (CoreUnfolding expr _ _ _ _) -> Def (PN _id) (convertExpression expr)
  where
    unfolding = realIdUnfolding _id
