{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CoreTranslate.Translate where

import CoreTranslate.Language
import GHC.Plugins
import Prelude hiding (id)

convertBinds :: [CoreBind] -> ProveLanguage
convertBinds = concatMap convertBind

convertBind :: CoreBind -> ProveLanguage
convertBind (NonRec id expr) = [convertIdExpression id expr]
convertBind (Rec l) =  concatMap (\(id, expr) -> convertBind (NonRec id expr)) l

convertIdExpression :: CoreBndr -> CoreExpr -> VariableDef
convertIdExpression name expr = Def (idToUniqueString name) (convertExpression expr)

idToUniqueString :: Id -> String
idToUniqueString v = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"

convertExpression :: CoreExpr -> ProveExpression
convertExpression (Var info)                            = Variable (idToUniqueString info) (idType info)
convertExpression (Lit lit)                             = Literal (convertLiteral lit) (convertLiteraltoType lit)
convertExpression (App expr arg)                        = DirectOperation (convertExpression expr) (convertExpression arg) (exprToType (App expr arg))
convertExpression (Lam varToBind expression)            = error "called convertExpression with Lambda"
convertExpression (Let _ _)                             = error "called convertExpression with Let"
convertExpression (GHC.Plugins.Case expr _ _ alts)      = error "called convertExpression with Case"
convertExpression (Cast _ _)                            = error "called convertExpression with Cast"
convertExpression (Tick _ expr)                         = convertExpression expr
convertExpression (Type t)                              = TypeVar t
convertExpression (Coercion _)                          = error "called convertExpression with Coercion"

convertLiteraltoType :: Literal -> Kind
convertLiteraltoType (LitChar c)         = error ""
convertLiteraltoType (LitNumber t v)     = error ""
convertLiteraltoType (LitString s)       = error ""
convertLiteraltoType (LitFloat r)        = error ""
convertLiteraltoType (LitDouble d)       = error ""

convertLiteral :: Literal -> LiteralTypes
convertLiteral (LitChar c)         = LChar c
convertLiteral (LitNumber t v)     = LNumber v
convertLiteral (LitString s)       = LString (show s)
convertLiteral (LitFloat r)        = LFloat r
convertLiteral (LitDouble d)       = LDouble d