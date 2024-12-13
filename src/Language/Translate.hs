{-# LANGUAGE BlockArguments #-}
module Language.Translate
  ( convertBinds,
    convertExpr,
  )
where

import Data.Type.Coercion (Coercion (Coercion))
import Debug.Trace (trace, traceShowId)
import qualified Debug.Trace as GP
import GHC.Core.TyCo.Rep (Coercion (FunCo))
import qualified GHC.Core.TyCo.Rep as GT
import qualified GHC.Plugins as GP
import Language.Expression
import GHC.IO (unsafePerformIO)
import GHC.Conc (threadDelay)

convertBinds ::
  (GP.HasCallStack) =>
  [GP.CoreBind] ->
  [Binding]
convertBinds =
  fmap convertBind

convertBind ::
  (GP.HasCallStack) =>
  GP.CoreBind ->
  Binding
convertBind
  (GP.NonRec _id expr) =
    trace (GP.showPprUnsafe $ GP.NonRec _id expr) $
      traceShowId $
        BindNonRec (convertVar _id) (convertExpr expr)
convertBind
  (GP.Rec bind) =
    BindRec $ map (\(a, b) -> (convertVar a, convertExpr b)) bind

{-# NOINLINE unsafeConvertLogger #-}
unsafeConvertLogger :: String -> Maybe ()
unsafeConvertLogger msg = seq (unsafePerformIO (appendFile "debug.log" msg))
   (Just undefined)

convertVar :: (GP.HasCallStack) => GP.Var -> VarRep
convertVar _id = VarRepped vName vShortName vType vUnfolding
  where
    vName = GP.nameStableString (GP.varName _id) ++ "(" ++ show (GP.getUnique _id) ++ ")"
    vShortName = GP.occNameString $ GP.occName $ GP.varName _id
    vType = convertType $ GP.varType _id
    vUnfolding = getUnfolding _id

getUnfolding :: (GP.HasCallStack) => GP.Var -> Maybe ExprRep
getUnfolding _id
  | GP.isId _id = do
      input_unfolding <- Just $ GP.realIdUnfolding _id
      unfolded <- GP.maybeUnfoldingTemplate input_unfolding
      converted <- Just $ convertExpr unfolded
      _ <- unsafeConvertLogger "conversion from:\n"
      _ <- unsafeConvertLogger (GP.showPprUnsafe input_unfolding ++ "\n")
      _ <- unsafeConvertLogger "\nunfolded:\n"
      _ <- unsafeConvertLogger (GP.showPprUnsafe unfolded ++ "\n")
      _ <- unsafeConvertLogger "conversion to:\n"
      _ <- unsafeConvertLogger (GP.showPprUnsafe (getGraph converted) ++ "\n")
      return $ converted
  | otherwise = Nothing

ownUnfolding :: GP.Unfolding -> Maybe GP.CoreExpr
ownUnfolding (GP.CoreUnfolding {GP.uf_tmpl = expr}) = Just expr
ownUnfolding (GP.DFunUnfolding {GP.df_bndrs = bndrs, GP.df_con = con, GP.df_args = args}) =
  Just $ GP.mkLams bndrs $ GP.mkApps (GP.mkLams (GP.dataConUnivTyVars con) $ GP.Var (GP.dataConWorkId con)) args
ownUnfolding _ = Nothing

convertArrow :: GP.FunTyFlag -> TypeFunctionForm
convertArrow GP.FTF_T_T = TypeToType
convertArrow GP.FTF_T_C = TypeToCoercion
convertArrow GP.FTF_C_T = CoercionToType
convertArrow GP.FTF_C_C = CoercionToCoercion

convertType :: (GP.HasCallStack) => GP.Kind -> TypeRep
convertType (GT.TyVarTy t) = TypeVariable $ convertVar t
convertType (GT.TyConApp _id args) =
  TypeConstructor
    ( GP.nameStableString $ GP.getName _id,
      map (convertVar . GP.binderVar) $ GP.tyConBinders _id,
      map (convertVar . GP.dataConWorkId) $ GP.tyConDataCons _id
    )
    (fmap convertType args)
convertType (GT.ForAllTy b t) = TypeForall (convertVar $ GP.binderVar b) (convertType t)
convertType (GT.FunTy af _ arg res) = TypeFunction (convertType arg) (convertArrow af) (convertType res)
convertType (GT.LitTy lit) = TypeLiteral $ convertTypeLit lit
convertType t@GT.AppTy {} = trace "not yet implemented convertType AppTy for: \n" undefined
convertType t@GT.CastTy {} = trace "not yet implemented convertType CastTy for: \n" undefined
convertType t@GT.CoercionTy {} = trace "not yet implemented convertType CastTy for: \n" undefined

convertTypeLit :: (GP.HasCallStack) => GT.TyLit -> LiteralRep
convertTypeLit (GT.NumTyLit n) = convertLit (GP.LitNumber GP.LitNumInt n)
convertTypeLit (GT.StrTyLit s) = convertLit (GP.LitString (GP.bytesFS s))
convertTypeLit (GT.CharTyLit c) = convertLit (GP.LitChar c)

convertCoercion :: (GP.HasCallStack) => GP.Coercion -> CoercionRep
convertCoercion (GT.Refl t) =
  CoRefl (convertType t)
convertCoercion (GT.GRefl _ t GT.MRefl) =
  CoRefl (convertType t)
convertCoercion (GT.GRefl _ t (GT.MCo c)) =
  CoGRefl (convertType t) (convertCoercion c)
convertCoercion (GT.TyConAppCo {}) = trace "not yet implemented convertCoercion for: TyConAppCo" CoercionUnknown
convertCoercion (GT.AppCo _ _) = trace "not yet implemented convertCoercion for: AppCo" CoercionUnknown
convertCoercion (GT.FunCo {GT.fco_arg = arg, GT.fco_res = res}) =
  CoFun (convertCoercion arg) (convertCoercion res)
convertCoercion (GT.ForAllCo var arg res) =
  CoForall (convertVar var) (convertCoercion arg) (convertCoercion res)
convertCoercion (GT.CoVarCo _) = trace "not yet implemented convertCoercion for: CoVarCo" CoercionUnknown
convertCoercion (GT.AxiomInstCo _ _ cs) = CoAxiomInst (map convertCoercion cs)
convertCoercion (GT.AxiomRuleCo _ _) = trace "not yet implemented convertCoercion for: AxiomRuleCo" CoercionUnknown
convertCoercion (GT.UnivCo {}) = trace "not yet implemented convertCoercion for: UnivCo" CoercionUnknown
convertCoercion (GT.SymCo c) = CoSym (convertCoercion c)
convertCoercion (GT.TransCo _ _) = trace "not yet implemented convertCoercion for: TransCo" CoercionUnknown
convertCoercion (GT.SelCo _ _) = trace "not yet implemented convertCoercion for: SelCo" CoercionUnknown
convertCoercion (GT.LRCo _ _) = trace "not yet implemented convertCoercion for: LRCo" CoercionUnknown
convertCoercion (GT.InstCo _ _) = trace "not yet implemented convertCoercion for: InstCo" CoercionUnknown
convertCoercion (GT.KindCo _) = trace "not yet implemented convertCoercion for: KindCo" CoercionUnknown
convertCoercion (GT.SubCo _) = trace "not yet implemented convertCoercion for: SubCo" CoercionUnknown
convertCoercion (GT.HoleCo _) = error "HoleCo should not exist on this level"

convertNumType ::
  (GP.HasCallStack) =>
  GP.LitNumType ->
  NumRep
convertNumType
  GP.LitNumBigNat =
    NLitNumBigNat
convertNumType
  GP.LitNumInt =
    NLitNumInt
convertNumType
  GP.LitNumInt8 =
    NLitNumInt8
convertNumType
  GP.LitNumInt16 =
    NLitNumInt16
convertNumType
  GP.LitNumInt32 =
    NLitNumInt32
convertNumType
  GP.LitNumInt64 =
    NLitNumInt64
convertNumType
  GP.LitNumWord =
    NLitNumWord
convertNumType
  GP.LitNumWord8 =
    NLitNumWord8
convertNumType
  GP.LitNumWord16 =
    NLitNumWord16
convertNumType
  GP.LitNumWord32 =
    NLitNumWord32
convertNumType
  GP.LitNumWord64 =
    NLitNumWord64

convertLit ::
  (GP.HasCallStack) =>
  GP.Literal ->
  LiteralRep
convertLit
  (GP.LitChar c) =
    LChar c
convertLit
  (GP.LitNumber numType v) =
    LNumber (convertNumType numType) v
convertLit
  (GP.LitString s) =
    LString $ drop 1 . init $ show s
convertLit
  (GP.LitFloat r) =
    LFloat r
convertLit
  (GP.LitDouble d) =
    LDouble d
convertLit
  _ =
    error "not supported literal"

convertExpr :: (GP.HasCallStack) => GP.CoreExpr -> ExprRep
convertExpr (GP.Var v) = ExprVar $ convertVar v
convertExpr (GP.Lit l) = ExprLit $ convertLit l
convertExpr (GP.App e a) = ExprApp (convertExpr e) (convertExpr a)
convertExpr (GP.Lam b e) = ExprLambda (convertVar b) (convertExpr e)
convertExpr (GP.Let b e) = ExprLet (convertBind b) (convertExpr e)
convertExpr (GP.Case m b t a) = ExprCase (convertExpr m) (convertVar b) (convertType t) (map convertAlt a)
convertExpr (GP.Cast e c) = convertExpr e --ExprCast (convertExpr e) (convertCoercion c)
convertExpr (GP.Tick _ e) = convertExpr e
convertExpr (GP.Type t) = ExprType $ convertType t
convertExpr (GP.Coercion c) = ExprCoer $ convertCoercion c

convertAlt :: (GP.HasCallStack) => GP.CoreAlt -> ExprAlt
convertAlt (GP.Alt a b c) =
  Alt (convertCon a) (fmap convertVar b) (convertExpr c)

convertCon :: (GP.HasCallStack) => GP.AltCon -> ExprCon
convertCon (GP.DataAlt con) = ExprDataCon $ convertVar $ GP.dataConWrapId con
convertCon (GP.LitAlt l) = ExprLitCon $ convertLit l
convertCon GP.DEFAULT = DefaultCon
