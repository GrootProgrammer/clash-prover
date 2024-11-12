{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CoreTranslate.Translate(convertBinds, convertExpression, replaceVariable, getExternalVariableDef) where

import Prelude
import CoreTranslate.Language hiding (arg, name, expr)
import GHC.Plugins

convertBinds :: [CoreBind] -> ProveLanguage
convertBinds = concatMap convertBind

convertBind :: CoreBind -> ProveLanguage
convertBind (NonRec id expr) = [convertIdExpression id expr]
convertBind (Rec l) = concatMap (\(id, expr) -> convertBind (NonRec id expr)) l

convertIdExpression :: CoreBndr -> CoreExpr -> VariableDef
convertIdExpression name expr = Def (PN name) (convertExpression expr)

replaceVariable :: ProveExpression -> ProveName -> ProveExpression -> ProveExpression
replaceVariable (Variable n) from to
    | n == from = to
    | otherwise = Variable n
replaceVariable (Literal l) _ _ = Literal l
replaceVariable (Lambda n e) from to
    | n == from = Lambda n e
    | otherwise = Lambda n (replaceVariable e from to)
replaceVariable (CoreTranslate.Language.Case e b alts) a (Variable c) = CoreTranslate.Language.Case (replaceVariable e a (Variable c)) (if a == c then b else a) (fmap (\(CI con binds ae) -> CI con binds (replaceVariable ae a (Variable c))) alts)
replaceVariable (CoreTranslate.Language.Case e b alts) from to = CoreTranslate.Language.Case (replaceVariable e from to) b (fmap (\(CI con binds ae) -> CI con binds (replaceVariable ae from to)) alts)
replaceVariable (DirectOperation dof a) from to = DirectOperation (replaceVariable dof from to) (replaceVariable a from to)

convertExpression :: CoreExpr -> ProveExpression
convertExpression (Var info)                            = Variable (PN info)
convertExpression (Lit lit)                             = Literal (convertLiteral lit)
convertExpression (App expr arg)                        = DirectOperation (convertExpression expr) (convertExpression arg)
convertExpression (Lam varToBind expression)            = Lambda (PN varToBind) (convertExpression expression)
convertExpression (Let (NonRec id expr) expression)     = replaceVariable (convertExpression expression) (PN id) (convertExpression expr)
convertExpression (GHC.Plugins.Case a b _ d)            = CoreTranslate.Language.Case (convertExpression a) (PN b) (fmap convertAlt d)
convertExpression (Cast e _)                            = convertExpression e
convertExpression (Tick _ expr)                         = convertExpression expr
convertExpression (Coercion _)                          = error "called convertExpression with Coercion"
convertExpression (Type t)                              = Literal $ Typed $ PT t
convertExpression t                                     = error $ "non exhaustive: " ++ showPprUnsafe t

convertAlt :: Alt CoreBndr -> CaseInstance
convertAlt (Alt con bind expr) = CI con (fmap PN bind) (convertExpression expr)

convertNumType :: LitNumType -> NumTypes 
convertNumType GHC.Plugins.LitNumBigNat = CoreTranslate.Language.LitNumBigNat 
convertNumType GHC.Plugins.LitNumInt    = CoreTranslate.Language.LitNumInt    
convertNumType GHC.Plugins.LitNumInt8   = CoreTranslate.Language.LitNumInt8   
convertNumType GHC.Plugins.LitNumInt16  = CoreTranslate.Language.LitNumInt16  
convertNumType GHC.Plugins.LitNumInt32  = CoreTranslate.Language.LitNumInt32  
convertNumType GHC.Plugins.LitNumInt64  = CoreTranslate.Language.LitNumInt64  
convertNumType GHC.Plugins.LitNumWord   = CoreTranslate.Language.LitNumWord   
convertNumType GHC.Plugins.LitNumWord8  = CoreTranslate.Language.LitNumWord8  
convertNumType GHC.Plugins.LitNumWord16 = CoreTranslate.Language.LitNumWord16 
convertNumType GHC.Plugins.LitNumWord32 = CoreTranslate.Language.LitNumWord32 
convertNumType GHC.Plugins.LitNumWord64 = CoreTranslate.Language.LitNumWord64 

convertLiteral :: Literal -> LiteralTypes
convertLiteral (LitChar c)
    = LChar c
convertLiteral (LitNumber numType v)
    = LNumber (convertNumType numType) v
convertLiteral (LitString s)
    = LString (show s)
convertLiteral (LitFloat r)
    = LFloat r
convertLiteral (LitDouble d)
    = LDouble d

--panics if ProveName is in the current Module
getExternalVariableDef :: [ProveExpression] -> ProveName -> VariableDef
getExternalVariableDef _ (PN id) = case unfolding of
    NoUnfolding -> Def (PN id) (Variable (PN id))
    BootUnfolding -> Def (PN id) (Variable (PN id))
    (OtherCon []) -> Def (PN id) (Literal (Constructor (PN id)))
    (OtherCon [DataAlt l]) -> Def (PN id) (Literal (Dict l [Variable (PN id)]))
    (OtherCon xs) -> error ("constructor with multiple options: " ++ showPprUnsafe (OtherCon xs))
    (DFunUnfolding [] c args) -> Def (PN id) (Literal (Dict c (fmap convertExpression args)))
    (DFunUnfolding vars c args) -> Def (PN id) (foldr (\v e -> Lambda (PN v) e) (Literal (Dict c (fmap convertExpression args))) vars)
    (CoreUnfolding expr _ _ _ _) -> Def (PN id) (convertExpression expr)
    where
        unfolding = realIdUnfolding id