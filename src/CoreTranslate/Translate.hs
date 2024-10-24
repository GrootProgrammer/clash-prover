{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module CoreTranslate.Translate(convertBinds, convertExpression) where

import Prelude
import CoreTranslate.Language hiding (arg, name, expr)
import GHC.Plugins

convertBinds :: [CoreBind] -> ProveLanguage
convertBinds = concatMap convertBind

convertBind :: CoreBind -> ProveLanguage
convertBind (NonRec id expr) = [convertIdExpression id expr]
convertBind (Rec l) =  concatMap (\(id, expr) -> convertBind (NonRec id expr)) l

convertIdExpression :: CoreBndr -> CoreExpr -> VariableDef
convertIdExpression name expr = Def (PN name) (convertExpression expr)

convertExpression :: CoreExpr -> ProveExpression
convertExpression (Var info)                            = Variable (PN info)
convertExpression (Lit lit)                             = Literal (convertLiteral lit)
convertExpression (App expr arg)                        = DirectOperation (convertExpression expr) (convertExpression arg)
convertExpression (Lam varToBind expression)            = Lambda (PN varToBind) (convertExpression expression)
convertExpression (Let (NonRec id expr) expression)     = convertExpression (App (Lam id expression) expr)
convertExpression (GHC.Plugins.Case {})                 = error "called convertExpression with Case"
convertExpression (Cast _ _)                            = error "called convertExpression with Cast"
convertExpression (Tick _ expr)                         = convertExpression expr
convertExpression (Type t)                              = TypeVar (PT t)
convertExpression (Coercion _)                          = error "called convertExpression with Coercion"

convertLiteraltoType :: Literal -> Kind
convertLiteraltoType _         = error ""

convertLiteral :: Literal -> LiteralTypes
convertLiteral (LitChar c)         = LChar c
convertLiteral (LitNumber _ v)     = LNumber v
convertLiteral (LitString s)       = LString (show s)
convertLiteral (LitFloat r)        = LFloat r
convertLiteral (LitDouble d)       = LDouble d