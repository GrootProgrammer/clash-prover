{-# LANGUAGE TupleSections #-}

module Rewrite.OperationalRewrite where

import Data.List (uncons)
import Data.List.Extra (snoc)
import Debug.Trace (trace)
import GHC.Data.Maybe
import Language.Expression
import Rewrite.Primitives (applyPrimRules, getPrimStackUsage, isPrimitive)

type OperationalRule = ExprRep -> Maybe ExprRep

operationalRules :: [(String, OperationalRule)]
operationalRules =
  [ ("sVar", sVar),
    ("sApp", sApp),
    ("sLambda", sLambda),
    ("sBeta", sBeta),
    ("sCase", sCase),
    ("sCaseMatch", sCaseMatch),
    ("sPrim", sPrim),
    ("sPrimStack", sPrimStack)
  ]

applyOpRules :: ExprRep -> Maybe ExprRep
applyOpRules expr = case new_expr of
  Nothing -> Nothing
  Just (name_r, expr_r) -> trace name_r $ Just expr_r
  where
    new_expr = firstJusts $ map (\(n, r) -> fmap (n,) (r expr)) operationalRules

sVar :: OperationalRule
sVar (ExprVar v)
  | isPrimitive v = Nothing
  | otherwise = varUnfolding v
sVar _ = Nothing

sTPush :: OperationalRule
sTPush (ExprApp (ExprVar v) (ExprType t)) = case varType v of
  TypeForall vmatch b -> Just $ ExprVar v {varType = alphaReduction b vmatch (ExprType t)}
  _ -> Nothing
sTPush _ = Nothing

sApp :: OperationalRule
sApp (ExprApp f a) = do
  f_r <- applyOpRules f
  return $ ExprApp f_r a
sApp _ = Nothing

sLambda :: OperationalRule
sLambda (ExprLambda n f) = do
  f_r <- applyOpRules f
  return $ ExprLambda n f_r
sLambda _ = Nothing

sBeta :: OperationalRule
sBeta (ExprApp (ExprLambda n e1) e2) = do
  return $ alphaReduction e1 n e2
sBeta _ = Nothing

sCase :: OperationalRule
sCase (ExprCase e a b c) = do
  e_r <- applyOpRules e
  return $ ExprCase e_r a b c
sCase _ = Nothing

getWHNF :: ExprRep -> Maybe (ExprRep, [ExprRep])
getWHNF (ExprVar v) = Just (ExprVar v, [])
getWHNF (ExprLit l) = Just (ExprLit l, [])
getWHNF (ExprApp f a) = do
  (childWHNF, args) <- getWHNF f
  return (childWHNF, snoc args a)
getWHNF _ = Nothing

sCaseMatch :: OperationalRule
sCaseMatch (ExprCase m v _ alt) = do
  (whnf, args) <- getWHNF m
  let matches = filter (\(Alt ma _ _) -> isEqual ma whnf) alt
  _ <- if length matches /= 1 then Nothing else pure undefined
  (Alt ma binds expr, _) <- uncons matches
  let stackVarsLength = length args - length binds
  _ <- if stackVarsLength < 0 then Nothing else pure undefined
  let stackVars = take stackVarsLength args
  let bindVars = drop stackVarsLength args
  let argbinds = zip bindVars binds
  return $
    foldr (\(a) acc -> ExprApp acc a) (foldr (\(a, b) e -> alphaReduction e b a) (alphaReduction expr v m) argbinds) stackVars
sCaseMatch _ = Nothing

sPrim :: OperationalRule
sPrim = applyPrimRules

sPrimStack :: OperationalRule
sPrimStack (ExprApp e a) = do
  _ <- getPrimStackUsage e
  a_r <- applyOpRules a
  return $ ExprApp e a_r
sPrimStack _ = Nothing

{-

type OperationalRule v t c = (v -> Maybe (ExprRep v t c)) -> ExprRep v t c -> Maybe (ExprRep v t c)

operationalRules :: (ExprGen v t c) => [(String, OperationalRule v t c)]
operationalRules =
  [ ("sVar", sVar),
    ("sApp", sApp),
    ("sBeta", sBeta),
    ("sPush", sPush),
    --    ("sTPush", sTPush)
    ("sCPush", sCPush),
    ("sTPush", sTPush),
    ("sTrans", sTrans)
  ]

applyOpRules :: (ExprGen v t c) => (v -> Maybe (ExprRep v t c)) -> ExprRep v t c -> Maybe (ExprRep v t c)
applyOpRules language expr = case new_expr of
  Nothing -> Nothing
  Just (name_r, expr_r) -> trace name_r $ Just expr_r
  where
    new_expr = firstJusts $ map (\(n, r) -> fmap (n,) (r language expr)) operationalRules

sVar :: (ExprGen v t c) => OperationalRule v t c
sVar lang (ExprVar n) = unfolded
  where
    unfolded = lang n
sVar _ _ = Nothing

sApp :: (ExprGen v t c) => OperationalRule v t c
sApp lang (ExprApp e1 e2) = fmap (`ExprApp` e2) e1'
  where
    e1' = applyOpRules lang e1
sApp _ _ = Nothing

sBeta :: (ExprGen v t c) => OperationalRule v t c
sBeta _ (ExprApp (ExprLambda n e1) e2) = Just $ exprAlphaConversion n e2 e1
sBeta _ _ = Nothing

sPush :: (ExprGen v t c) => OperationalRule v t c
sPush _ (ExprApp (ExprCast (ExprLambda _ _) _) (ExprType _)) = Nothing
sPush _ (ExprApp (ExprCast (ExprLambda _ _) _) (ExprCoer _)) = Nothing
sPush _ (ExprApp (ExprCast (ExprLambda n e1) gamma) e2) = sPushExpr gamma_0 gamma_1
  where
    sPushExpr (Just g0) (Just g1) = Just $ ExprApp (ExprLambda n $ ExprCast e1 g1) (ExprCast e2 g0)
    sPushExpr _ _ = Nothing
    gamma_0 = getSym <$> getNth 0 gamma
    gamma_1 = getNth 1 gamma
sPush _ _ = Nothing

sTPush :: (ExprGen v t c) => OperationalRule v t c
sTPush _ _ =
  --  ExprApp (ExprLambda n (ExprApp n)) (ExprType tau)
  --  This rule is unclear in the order of operations
  Nothing

sCPush :: (ExprGen v t c) => OperationalRule v t c
sCPush
  _
  (ExprApp (ExprCast (ExprLambda n e) gamma) (ExprCoer gamma')) =
    cPushExpr gamma_0 gamma_1 gamma_2
    where
      cPushExpr (Just g0) (Just g1) (Just g2) = Just $ ExprApp (ExprLambda n (ExprCast e g2)) (ExprCoer (g0 ； gamma' ； g1))
      cPushExpr _ _ _ = Nothing
      gamma_0 = getNth 1 =<< getNth 0 gamma
      gamma_1 = getSym <$> (getNth 2 =<< getNth 0 gamma)
      gamma_2 = getNth 1 gamma
sCPush _ _ = Nothing

sTrans :: (ExprGen v t c) => OperationalRule v t c
sTrans _ (ExprCast (ExprCast e g1) g2) = Just $ ExprCast e (g1 ； g2)
sTrans _ _ = Nothing

sCast :: (ExprGen v t c) => OperationalRule v t c
sCast lang (ExprCast e g) = (`ExprCast` g) <$> applyOpRules lang e
sCast _ _ = Nothing

sCase :: (ExprGen v t c) => OperationalRule v t c
sCase lang (ExprCase e n t alts) = (\e' -> ExprCase e' n t alts) <$> rewritten
  where
    rewritten = applyOpRules lang e
sCase _ _ = Nothing

sMatchCase :: (ExprGen v t c) => OperationalRule v t c
-- looks more imtimidating
sMatchCase _ _ = Nothing

sMatchLit :: (ExprGen v t c) => OperationalRule v t c
sMatchLit _ _ = Nothing

sMatchDefault :: (ExprGen v t c) => OperationalRule v t c
sMatchDefault _ _ = Nothing

sCasePush :: (ExprGen v t c) => OperationalRule v t c
sCasePush _ _ = Nothing

sLetUnused :: (ExprGen v t c) => OperationalRule v t c
sLetUnused _ (ExprLet b e)
  | disjoint (getVarsExpr e) (getVarsBind b) = Just e
  | otherwise = Nothing
sLetUnused _ _ = Nothing

sLet :: (ExprGen v t c) => OperationalRule v t c
sLet lang (ExprLet b e) = new_expr
  where
    new_expr = applyOpRules new_lookup e
    new_lookup v = maybe (lang v) Just (lookupVar b v)
sLet _ _ = Nothing

-}
