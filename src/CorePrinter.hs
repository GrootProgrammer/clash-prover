module CorePrinter (printCode) where

import Prelude
import GHC.Plugins
import Data.List (intercalate)
import GHC.Core.TyCo.Rep
import Prelude hiding (id)

printCode :: CoreBind -> CoreM ()
printCode (NonRec id expr) = do
  liftIO $ putStrLn "NonRec:"
  liftIO $ putStrLn $ printFuncName id ++ " :: " ++ printFuncType id
  liftIO $ putStrLn  (printFuncName id ++ " = \n" ++ printExpr 1 expr)
  putMsg $ ppr expr
  liftIO $ putStrLn  ""
printCode (Rec l) = do
  liftIO $ putStrLn "Recursive binder: "
  liftIO $ putStrLn $ intercalate "\n" $ map (\(id, expr) -> indent 0 ++ printFuncName id ++ " :: " ++ printFuncType id ++ "\n" ++ printFuncName id ++ " = \n" ++ printExpr 1 expr ) l
  mapM_ (\(_, expr) -> putMsg $ ppr expr ) l

printFuncName :: CoreBndr -> String
--slightly uglier but unique if combined with the getUnique
printFuncName v = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"
printFuncType :: CoreBndr -> String
printFuncType v = printExprKind 0 (varType v)

printVarName :: Var -> String
printVarName v = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"

indent :: Integer -> String
indent i
  | i <= 0 = ""
  | otherwise = "\t" ++ indent (i-1)

printFlag :: FunTyFlag -> String
printFlag FTF_T_T  = "->"
printFlag FTF_T_C  = "-=>"
printFlag FTF_C_T  = "=>"
printFlag FTF_C_C  = "==>"

printExprKind :: Integer -> Type -> String
printExprKind level (TyVarTy v) = indent level ++ show (nameStableString $ getName v)
printExprKind level (AppTy f a) = indent level ++ printExprKind 0 f ++ "," ++ printExprKind 0 a
printExprKind level (TyConApp tyCon []) = indent level ++ show (nameStableString $ getName tyCon)
printExprKind level (TyConApp tyCon kot) = indent level ++ "(" ++ show (nameStableString $ getName tyCon) ++ " @" ++ intercalate " @" (map (printExprKind 0) kot) ++ ")"
printExprKind level (ForAllTy (Bndr id _) t) = indent level ++ "(" ++ "forall " ++ printFuncDef 0 id ++ "). " ++ printExprKind 0 t
printExprKind level (FunTy flag _ arg res) = indent level ++ printExprKind 0 arg ++ " " ++ printFlag flag ++ " " ++ printExprKind 0 res
printExprKind level (LitTy (NumTyLit i)) = indent level ++ "(" ++ show i ++ " :: Integer)"
printExprKind level (LitTy (StrTyLit i)) = indent level ++ "(" ++ show i ++ " :: [Char])"
printExprKind level (LitTy (CharTyLit i)) = indent level ++ "(" ++ show i ++ " :: Char)"
printExprKind _ (CastTy _ _) = "CastTy"
printExprKind _ (CoercionTy _) = "CoercionTy"

printFuncDef :: Integer -> CoreBndr -> String
printFuncDef level v = indent level ++ show (nameStableString $ getName v) ++ " :: " ++ printExprKind 0 (varType v)

printNumType :: LitNumType -> TyCon
printNumType LitNumBigNat = naturalTyCon
printNumType LitNumInt    = intTyCon
printNumType LitNumInt8   = tyConAppTyCon int8RepDataConTy
printNumType LitNumInt16  = tyConAppTyCon int16RepDataConTy
printNumType LitNumInt32  = tyConAppTyCon int32RepDataConTy
printNumType LitNumInt64  = tyConAppTyCon int64RepDataConTy
printNumType LitNumWord   = wordTyCon
printNumType LitNumWord8  = word8TyCon
printNumType LitNumWord16 = tyConAppTyCon word16RepDataConTy
printNumType LitNumWord32 = tyConAppTyCon word32RepDataConTy
printNumType LitNumWord64 = tyConAppTyCon word64RepDataConTy

printLiteral :: Literal -> String
printLiteral (LitChar c)         = show c ++ " :: " ++ printExprKind 0 (mkTyConApp charTyCon [])
printLiteral (LitNumber t v)     = show v ++ " :: " ++ printExprKind 0 (mkTyConApp (printNumType t) [])
printLiteral (LitString s)       = show s ++ " :: " ++ printExprKind 0 (mkTyConApp listTyCon [mkTyConApp charTyCon []])
printLiteral (LitFloat r)        = show r ++ " :: " ++ printExprKind 0 (mkTyConApp floatTyCon [])
printLiteral (LitDouble d)       = show d  ++ " :: " ++ printExprKind 0 (mkTyConApp doubleTyCon [])
printLiteral _                  = "Unsupported subexpression"

---- Pretty-printing Core expressions
printExpr :: Integer -> CoreExpr -> String
printExpr level (Var v)                   = indent level ++ printFuncName v
printExpr level (Lit l)                   = indent level ++ printLiteral l
printExpr level (App e1 e2)               = indent level ++ printExpr 0 e1 ++ " " ++ printExpr 0 e2
printExpr level (Lam b expr)              = indent level ++ "(\\" ++ printVarName b ++ " -> " ++ "\n" ++ printExpr (level + 1) expr ++ ")"
printExpr level (Let _ _)                 = indent level ++ "Unsupported expression: Let"  --indent level ++ "let " ++ printBinders' bind ++ " in " ++ printExpr 0 expr
printExpr level (Case expr _ _ alts)      = indent level ++ "case " ++ printExpr 0 expr ++ " of " ++ "\n" ++ printAlts (level+1) alts
printExpr level (Cast _ _)                = indent level ++ "Unsupported expression: Cast" -- printExpr level expr ++ " -- Cast"
printExpr level (Tick _ expr)             = printExpr level expr
printExpr level (Type t)                  = indent level ++ "@" ++ printExprKind 0 t
printExpr level (Coercion _)              = indent level ++ "Unsupported expression: Coercion" -- indent level ++ "Coercion"
--printExpr level _                         = indent level ++ "Unsupported expression"

-- Pretty-printing alternatives in a Case expression
printAlts :: Integer -> [CoreAlt] -> String
printAlts level alts = intercalate "\n" $ map (printAlt level) alts

printAlt :: Integer -> CoreAlt -> String
printAlt level (Alt (DataAlt d) bndrs expr) =
  indent level ++ show (nameStableString $ getName d) ++ " " ++ unwords (map printVarName bndrs) ++ " -> \n" ++ printExpr (level+1) expr
printAlt level (Alt (LitAlt l) bndrs expr) =
  indent level ++ printLiteral l ++ " " ++ unwords (map printVarName bndrs) ++ " -> \n" ++ printExpr (level+1) expr
printAlt level (Alt DEFAULT bndrs expr) =
  indent level ++ "default" ++ " " ++ unwords (map printVarName bndrs) ++ " -> \n" ++ printExpr (level+1) expr