{-# LANGUAGE DeriveDataTypeable #-}
module CorePrinter (printBinders, printCode) where
import GHC.Plugins
import Data.Data
import Control.Monad
import Data.List (intercalate)
import GHC.Core.TyCo.Rep
import GHC.Types.Var

printCode :: CoreBind -> CoreM ()
printCode (NonRec id expr) = do
  liftIO $ putStrLn  (printFuncDef 0 id)
  liftIO $ putStrLn  (printFunctionName id ++ " = " ++ printExpr 0 expr)
  liftIO $ putStrLn  ""
printCode (Rec l) = do
  liftIO $ putStrLn "Recursive binder: "
  liftIO $ putStrLn $ intercalate "\n" $ map (\(id, expr) -> printFuncDef 1 id ++ "\n" ++ printExpr 2 expr ++ "\n") l

printFunctionName :: Id -> String
printFunctionName v = show (nameStableString $ getName v)

printBinders :: CoreBind -> CoreM ()
printBinders (NonRec id expr) = do
  liftIO $ putStrLn  (printFuncDef 0 id)
printBinders (Rec l) = do
  liftIO $ putStrLn "Recursive binder: "
  liftIO $ putStrLn $ intercalate "\n" $ map (\(id, expr) -> printFuncDef 1 id) l

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

printNumType :: LitNumType -> String
printNumType LitNumBigNat = "BigNat"
printNumType LitNumInt    = "Int"
printNumType LitNumInt8   = "Int8"
printNumType LitNumInt16  = "Int16"
printNumType LitNumInt32  = "Int32"
printNumType LitNumInt64  = "Int64"
printNumType LitNumWord   = "Word"
printNumType LitNumWord8  = "Word8"
printNumType LitNumWord16 = "Word16"
printNumType LitNumWord32 = "Word32"
printNumType LitNumWord64 = "Word64"

printExpr :: Integer -> CoreExpr -> String
printExpr level (Var v)                   = indent level ++ show (nameStableString $ getName v)
printExpr level (Lit (LitChar c))         = indent level ++ show c ++ " :: Char"
printExpr level (Lit (LitNumber t v))     = indent level ++ show v ++ " :: " ++ printNumType t
printExpr level (Lit (LitString  s))      = indent level ++ show s ++ " :: String"
printExpr level (Lit LitNullAddr)         = indent level ++ "null" ++ " :: Ptr"
printExpr level (Lit (LitRubbish tc rt))  = indent level ++ "bullshit :: Bullshit"
printExpr level (Lit (LitFloat  r))       = indent level ++ show r ++ " :: Float"
printExpr level (Lit (LitDouble d))       = indent level ++ show d ++ " :: Double"
printExpr level (Lit (LitLabel  fs mi f)) = indent level ++ "Unsupported subexpression"
printExpr level (App e1 e2)               = indent level ++ printExpr 0 e1 ++ " (" ++ printExpr 0 e2 ++ ")"
printExpr level (Lam b expr)              = indent level ++ "(\\" ++ printFunctionName b ++ " -> " ++ printExpr 0 expr  ++ ")"
printExpr level (Case expr b t alts) = indent level ++ "Unsupported expression"
--tick is for debugging purposes, irrelevent for most other cases
printExpr level (Tick _ expr)             = printExpr level expr
printExpr level _                         = indent level ++ "Unsupported expression"
--printExpr :: Integer -> CoreExpr -> String
--printExpr (Lit l)           = putMsg $ text "Lit: " <+> ppr l
--printExpr (App e1 e2)       = do
--    putMsg $ text "App:"
--    printExpr e1
--    printExpr e2
--printExpr (Lam b e)         = do
--    putMsg $ text "Lam: " <+> ppr b
--    printExpr e
--printExpr (Let bind body)   = do
--    putMsg $ text "Let binding:"
--    printExpr body
--printExpr (Case scrut b ty alts) = do
--    putMsg $ text "Case:"
--    printExpr scrut
--    mapM_ (\(_, _, alt) -> printExpr alt) alts  -- Extracting the CoreExpr from the Alt tuple
--printExpr (Cast e c)        = do
--    putMsg $ text "Cast:"
--    printExpr e
--printExpr (Tick _ e)        = do
--    putMsg $ text "Tick:"
--    printExpr e
--printExpr (Type t)          = putMsg $ text "Type: " <+> ppr t
--printExpr (Coercion co)     = putMsg $ text "Coercion: " <+> ppr co