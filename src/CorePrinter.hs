module CorePrinter (printCode) where
import GHC.Plugins
import Data.List (intercalate)
import GHC.Core.TyCo.Rep
import Prelude hiding (id)

printCode :: CoreBind -> CoreM ()
printCode (NonRec id expr) = do
  liftIO $ putStrLn "NonRec:"
  liftIO $ putStrLn $ printFuncName id ++ " :: " ++ printFuncType id
--  liftIO $ putStrLn  (printVarName id ++ " = " ++ printExpr 0 expr)
  putMsg $ ppr (NonRec id expr)
  liftIO $ putStrLn  ""
printCode (Rec l) = do
  liftIO $ putStrLn "Recursive binder: "
  putMsg $ ppr (Rec l)
--  liftIO $ putStrLn $ intercalate "\n" $ map (\(id, expr) -> indent 1 ++ printVarDef id ++ "\n" ++ printExpr 2 expr ++ "\n") l

--codeString :: CoreBind -> String
--codeString (NonRec id expr) = do
--  printFuncName id ++ "\n" ++ (printVarName id ++ " = " ++ printExpr 0 expr)
--codeString (Rec l) = do
--  "Recursive binder: \n" ++ intercalate "\n" (map (\(id, expr) -> indent 1 ++ printVarDef id ++ "\n" ++ printExpr 2 expr ++ "\n") l)
--
printFuncName :: Var -> String
printFuncName v = nameStableString $ getName v
printFuncType :: Var -> String
printFuncType v = printExprKind 0 (varType v)
--
--printVarName :: Var -> String
--printVarName v = nameStableString $ getName v
--
--printVarType :: Var -> String
--printVarType v = printExprKind 0 (varType v)
--
--printVarDef :: Var -> String
--printVarDef v = printVarName v ++ " :: " ++ printVarType v
--
--printBinders :: CoreBind -> CoreM ()
--printBinders (NonRec id expr) = do
--  liftIO $ putStrLn  (printFuncDef 0 id)
--printBinders (Rec l) = do
--  liftIO $ putStrLn "Recursive binder: "
--  liftIO $ putStrLn $ intercalate "\n" $ map (\(id, expr) -> printFuncDef 1 id) l
--
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
printExprKind level (LitTy (NumTyLit i)) = indent level ++ show i
printExprKind level (LitTy (StrTyLit i)) = indent level ++ show i
printExprKind level (LitTy (CharTyLit i)) = indent level ++ show i
printExprKind level (CastTy _ _) = "CastTy"
printExprKind level (CoercionTy _) = "CoercionTy"

printFuncDef :: Integer -> Var -> String
printFuncDef level v = indent level ++ show (nameStableString $ getName v) ++ " :: " ++ printExprKind 0 (varType v)
--
--printNumType :: LitNumType -> String
--printNumType LitNumBigNat = "BigNat"
--printNumType LitNumInt    = "Int"
--printNumType LitNumInt8   = "Int8"
--printNumType LitNumInt16  = "Int16"
--printNumType LitNumInt32  = "Int32"
--printNumType LitNumInt64  = "Int64"
--printNumType LitNumWord   = "Word"
--printNumType LitNumWord8  = "Word8"
--printNumType LitNumWord16 = "Word16"
--printNumType LitNumWord32 = "Word32"
--printNumType LitNumWord64 = "Word64"
--
--printLiteral :: Literal -> String
--printLiteral (LitChar c)         = show c ++ " :: " ++ printExprKind 0 (mkTyConApp charTyCon [])
--printLiteral (LitNumber t v)     = show v ++ " :: " ++ printExprKind 0 (mkTyConApp integerTyCon [])
--printLiteral (LitString s)       = show s ++ " :: " ++ printExprKind 0 (mkTyConApp listTyCon [mkTyConApp charTyCon []])
--printLiteral LitNullAddr         = "null :: Ptr"
--printLiteral (LitRubbish _ _)    = "bullshit :: Bullshit"
--printLiteral (LitFloat r)        = show r ++ " :: " ++ printExprKind 0 (mkTyConApp floatTyCon [])
--printLiteral (LitDouble d)       = show d  ++ " :: " ++ printExprKind 0 (mkTyConApp doubleTyCon [])
--printLiteral (LitLabel _ _ _)    = "Unsupported subexpression"
--
---- Pretty-printing Core expressions
--printExpr :: Integer -> CoreExpr -> String
--printExpr level (Var v)                   = indent level ++ printFuncName v
--printExpr level (Lit l)                   = indent level ++ printLiteral l
--printExpr level (App e1 e2)               = indent level ++ printExpr 0 e1 ++ " (" ++ printExpr 0 e2 ++ ")"
--printExpr level (Lam b expr)              = indent level ++ "(\\" ++ printVarName b ++ " -> " ++ printExpr 0 expr ++ ")"
--printExpr level (Let bind expr)           = indent level ++ "let " ++ printBinders' bind ++ " in " ++ printExpr 0 expr
--printExpr level (Case expr b _ alts)      = indent level ++ "case " ++ printExpr 0 expr ++ " of " ++ printVarName b ++ " -> " ++ printAlts level alts
--printExpr level (Cast expr _)             = printExpr level expr ++ " -- Cast"
--printExpr level (Tick _ expr)             = printExpr level expr -- Tick is often used for annotations, so we skip it
--printExpr level (Type _)                  = indent level ++ "Type"
--printExpr level (Coercion _)              = indent level ++ "Coercion"
----printExpr level _                         = indent level ++ "Unsupported expression"
--
---- Pretty-printing alternatives in a Case expression
--printAlts :: Integer -> [CoreAlt] -> String
--printAlts level alts = intercalate "\n" $ map (printAlt level) alts
--
--printAlt :: Integer -> CoreAlt -> String
--printAlt level (Alt (DataAlt d) bndrs expr) =
--  indent level ++ show (nameStableString $ getName d) ++ " " ++ unwords (map printVarName bndrs) ++ " -> " ++ printExpr (level + 1) expr
--printAlt level (Alt (LitAlt l) bndrs expr) =
--  indent level ++ printLiteral l ++ " " ++ unwords (map printVarName bndrs) ++ " -> " ++ printExpr (level + 1) expr
--printAlt level (Alt DEFAULT bndrs expr) =
--  indent level ++ "default" ++ " " ++ unwords (map printVarName bndrs) ++ " -> " ++ printExpr (level + 1) expr
--
---- Print binders in Let expressions
--printBinders' :: CoreBind -> String
--printBinders' (NonRec id expr) = printVarName id ++ " = " ++ printExpr 0 expr
--printBinders' (Rec l) = intercalate "; " $ map (\(id, expr) -> printVarName id ++ " = " ++ printExpr 0 expr) l