module Language.Translate
  ( convertBinds,
    convertExpression,
    getVariableDef,
    rerollStack,
    isPrimitive,
  )
where

import qualified GHC.Plugins as GP
import Language.LanguageUtils
import Language.Types
import Prelude

convertBinds :: (LanguageName l) => [GP.CoreBind] -> ProveLanguage l
convertBinds = concatMap convertBind

convertBind :: (LanguageName l) => GP.CoreBind -> ProveLanguage l
convertBind (GP.NonRec _id expr) = [convertIdExpression _id expr]
convertBind (GP.Rec l) = concatMap (\(_id, expr) -> convertBind (GP.NonRec _id expr)) l

convertIdExpression :: (LanguageName l) => GP.CoreBndr -> GP.CoreExpr -> VariableDef l
convertIdExpression name expr = Def ((convert . PN) name) (convertExpression expr)

convertExpression :: (LanguageName l) => GP.CoreExpr -> ProveExpression l
convertExpression (GP.Var info) = Variable ((convert . PN) info)
convertExpression (GP.Lit lit) = Literal $ convertLiteral lit
convertExpression (GP.App expr arg) = DirectOperation (convertExpression expr) (convertExpression arg)
convertExpression (GP.Lam varToBind expression) = Lambda ((convert . PN) varToBind) (convertExpression expression)
convertExpression (GP.Let (GP.NonRec _id expr) expression) = Let (convertIdExpression _id expr) (convertExpression expression)
convertExpression (GP.Let (GP.Rec ((_id, expr):xs)) expression) = Let (convertIdExpression _id expr) (convertExpression (GP.Let (GP.Rec xs) expression))
convertExpression (GP.Let (GP.Rec []) expression) = convertExpression expression
convertExpression (GP.Case a b _ d) = Case (convertExpression a) ((convert . PN) b) (fmap convertAlt d)
convertExpression (GP.Cast e _) = convertExpression e
convertExpression (GP.Tick _ expr) = convertExpression expr
convertExpression (GP.Coercion c) = Literal Coercion
convertExpression (GP.Type t) = Literal $ Typed $ PT t
convertExpression t = error $ "non exhaustive: " ++ GP.showPprUnsafe t

convertAltCon :: (LanguageName l) => GP.AltCon -> CaseMatch l
convertAltCon GP.DEFAULT = DEFAULT
convertAltCon (GP.DataAlt d) = Cons $ getProveNameForDatacon d
convertAltCon (GP.LitAlt l) = CaseLit $ convertLiteral l

convertAlt :: (LanguageName l) => GP.Alt GP.CoreBndr -> CaseInstance l
convertAlt (GP.Alt con bind expr) = CI (convertAltCon con) (fmap (convert . PN) bind) (convertExpression expr)

convertNumType :: GP.LitNumType -> NumTypes
convertNumType GP.LitNumBigNat = NLitNumBigNat
convertNumType GP.LitNumInt = NLitNumInt
convertNumType GP.LitNumInt8 = NLitNumInt8
convertNumType GP.LitNumInt16 = NLitNumInt16
convertNumType GP.LitNumInt32 = NLitNumInt32
convertNumType GP.LitNumInt64 = NLitNumInt64
convertNumType GP.LitNumWord = NLitNumWord
convertNumType GP.LitNumWord8 = NLitNumWord8
convertNumType GP.LitNumWord16 = NLitNumWord16
convertNumType GP.LitNumWord32 = NLitNumWord32
convertNumType GP.LitNumWord64 = NLitNumWord64

convertLiteral :: GP.Literal -> LiteralTypes n
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

isPrimitive :: (LanguageName a) => VariableDef a -> Bool
isPrimitive (Def n1 (Variable n2)) = n1 == n2
isPrimitive _ = False

-- panics if ProveName is in the current Module
getExternalVariableDef :: (LanguageName l) => [ProveExpression l] -> l -> VariableDef l
getExternalVariableDef _ var = case unfolding of
  GP.NoUnfolding -> Def (convert $ PN _id) (Variable (convert $ PN _id))
  GP.BootUnfolding -> Def (convert $ PN _id) (Variable (convert $ PN _id))
  (GP.OtherCon []) -> Def (convert $ PN _id) (Literal (Constructor (convert $ PN _id)))
  (GP.OtherCon [GP.DataAlt l]) -> Def (convert $ PN _id) (Literal (Constructor (getProveNameForDatacon l)))
  (GP.OtherCon xs) -> error ("constructor with multiple options: " ++ GP.showPprUnsafe (GP.OtherCon xs))
  (GP.DFunUnfolding vars c args) -> Def (convert $ PN _id) (lambda_tree $ do_tree base_of_tree)
    where
      lambda_tree i = foldl (flip Lambda) i (fmap (convert . PN) vars)
      do_tree i = foldl DirectOperation i leafs
      leafs = map convertExpression args
      base_of_tree = Literal $ Constructor (getProveNameForDatacon c)
  (GP.CoreUnfolding expr _ _ _ _) -> Def (convert $ PN _id) (convertExpression expr)
  where
    (PN _id) = getProveName var
    unfolding = GP.realIdUnfolding _id

getVariableDef :: (LanguageName l) => [VariableDef l] -> [ProveExpression l] -> l -> VariableDef l
getVariableDef (x : xs) context n
  | defName x == n = x
  | otherwise = getVariableDef xs context n
getVariableDef [] context n = getExternalVariableDef context n

rerollStack :: (Foldable t, LanguageName l) => t (ProveExpression l) -> ProveExpression l -> ProveExpression l
rerollStack xs e = foldl DirectOperation e xs
