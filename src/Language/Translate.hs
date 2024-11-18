module Language.Translate (
  convertBinds,
  convertExpression,
  getVariableDef,
  rerollStack
) where

import Language.Language
import Language.LanguageUtils
import qualified GHC.Plugins as GP
import Prelude

convertBinds :: [GP.CoreBind] -> ProveLanguage
convertBinds = concatMap convertBind

convertBind :: GP.CoreBind -> ProveLanguage
convertBind (GP.NonRec _id expr) = [convertIdExpression _id expr]
convertBind (GP.Rec l) = concatMap (\(_id, expr) -> convertBind (GP.NonRec _id expr)) l

convertIdExpression :: GP.CoreBndr -> GP.CoreExpr -> VariableDef
convertIdExpression name expr = Def (PN name) (convertExpression expr)

convertExpression :: GP.CoreExpr -> ProveExpression
convertExpression (GP.Var info) = Variable (PN info)
convertExpression (GP.Lit lit) = Literal $ convertLiteral lit
convertExpression (GP.App expr arg) = DirectOperation (convertExpression expr) (convertExpression arg)
convertExpression (GP.Lam varToBind expression) = Lambda (PN varToBind) (convertExpression expression)
convertExpression (GP.Let (GP.NonRec _id expr) expression) = Let (convertIdExpression _id expr) (convertExpression expression)
convertExpression (GP.Case a b _ d) = Case (convertExpression a) (PN b) (fmap convertAlt d)
convertExpression (GP.Cast e _) = convertExpression e
convertExpression (GP.Tick _ expr) = convertExpression expr
convertExpression (GP.Coercion _) = error "called convertExpression with Coercion"
convertExpression (GP.Type t) = Literal $ Typed $ PT t
convertExpression t = error $ "non exhaustive: " ++ GP.showPprUnsafe t

convertAltCon :: GP.AltCon -> CaseMatch
convertAltCon GP.DEFAULT = DEFAULT
convertAltCon (GP.DataAlt d) = Cons $ getProveNameForDatacon d
convertAltCon (GP.LitAlt l) = CaseLit $ convertLiteral l

convertAlt :: GP.Alt GP.CoreBndr -> CaseInstance
convertAlt (GP.Alt con bind expr) = CI (convertAltCon con) (fmap PN bind) (convertExpression expr)

convertNumType :: GP.LitNumType -> NumTypes
convertNumType GP.LitNumBigNat  = NLitNumBigNat
convertNumType GP.LitNumInt     = NLitNumInt
convertNumType GP.LitNumInt8    = NLitNumInt8
convertNumType GP.LitNumInt16   = NLitNumInt16
convertNumType GP.LitNumInt32   = NLitNumInt32
convertNumType GP.LitNumInt64   = NLitNumInt64
convertNumType GP.LitNumWord    = NLitNumWord
convertNumType GP.LitNumWord8   = NLitNumWord8
convertNumType GP.LitNumWord16  = NLitNumWord16
convertNumType GP.LitNumWord32  = NLitNumWord32
convertNumType GP.LitNumWord64  = NLitNumWord64

convertLiteral :: GP.Literal -> LiteralTypes
convertLiteral (GP.LitChar c) =
  LChar c
convertLiteral (GP.LitNumber numType v) =
  LNumber (convertNumType numType) v
convertLiteral (GP.LitString s) =
  LString $ show s
convertLiteral (GP.LitFloat r) =
  LFloat r
convertLiteral (GP.LitDouble d) =
  LDouble d
convertLiteral _ = error "not supported literal"

-- panics if ProveName is in the current Module
getExternalVariableDef :: [ProveExpression] -> ProveName -> VariableDef
getExternalVariableDef _ (PN _id) = case unfolding of
  GP.NoUnfolding -> Def (PN _id) (Variable (PN _id))
  GP.BootUnfolding -> Def (PN _id) (Variable (PN _id))
  (GP.OtherCon []) -> Def (PN _id) (Literal (Constructor (PN _id)))
  (GP.OtherCon [GP.DataAlt l]) -> Def (PN _id) (Literal (Constructor $ getProveNameForDatacon l))
  (GP.OtherCon xs) -> error ("constructor with multiple options: " ++ GP.showPprUnsafe (GP.OtherCon xs))
  (GP.DFunUnfolding vars c args) -> Def (PN _id) (foldr (Lambda . PN) (rerollStack (fmap convertExpression args) $ Literal $ Constructor $ getProveNameForDatacon c) vars)
  (GP.CoreUnfolding expr _ _ _ _) -> Def (PN _id) (convertExpression expr)
  where
    unfolding = GP.realIdUnfolding _id

getVariableDef :: ProveLanguage -> [ProveExpression] -> ProveName -> VariableDef
getVariableDef (x:xs) context n
    | defName x == n = x
    | otherwise = getVariableDef xs context n
getVariableDef [] context n = getExternalVariableDef context n

rerollStack :: [ProveExpression] -> ProveExpression -> ProveExpression
rerollStack xs e = foldl DirectOperation e xs
