{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Expression where

import Data.Int
import Data.Maybe (isJust)
import qualified Data.Set as DS
import Data.Word
import Utils.ToGraphviz (GraphvizNode (Node))

data NumRep
  = -- | Integer type
    NLitNumInteger
  | -- | Natural number type
    NLitNumNatural
  | -- | Arbitrary natural numbers, no bound
    NLitNumBigNat
  | -- | Standard Int type
    NLitNumInt
  | -- | 8-bit Int type
    NLitNumInt8
  | -- | 16-bit Int type
    NLitNumInt16
  | -- | 32-bit Int type
    NLitNumInt32
  | -- | 64-bit Int type
    NLitNumInt64
  | -- | Word type (unsigned integer)
    NLitNumWord
  | -- | 8-bit Word type
    NLitNumWord8
  | -- | 16-bit Word type
    NLitNumWord16
  | -- | 32-bit Word type
    NLitNumWord32
  | -- | 64-bit Word type
    NLitNumWord64
  deriving (Show, Eq)

listNodes :: [GraphvizNode] -> GraphvizNode
listNodes = listNodesS "[]"

listNodesS :: String -> [GraphvizNode] -> GraphvizNode
listNodesS d xs = Node d $ zipWith (\n (i :: Integer) -> (show i, n)) xs [0 ..]

lookIntoBreak :: DS.Set VarRep -> VarRep -> (DS.Set VarRep -> DS.Set VarRep) -> DS.Set VarRep
lookIntoBreak discovered v f = if v `elem` discovered then discovered else f $ DS.insert v discovered

class CoreRep a where
  alphaReduction :: a -> VarRep -> ExprRep -> a
  getBinds :: DS.Set VarRep -> a -> DS.Set VarRep

  -- used for logging
  getGraph :: a -> GraphvizNode

instance (CoreRep a) => CoreRep [a] where
  alphaReduction i from to = fmap (\e -> alphaReduction e from to) i
  getGraph l = Node "[]" $ zipWith (\e (i :: Integer) -> (show i, getGraph e)) l [0 ..]
  getBinds = foldr (flip getBinds)

-- Put all functions to do with NumRep here:

instance CoreRep LiteralRep where
  alphaReduction l _ _ = l
  getGraph (LString l) = Node l []
  getGraph l = Node (show l) []
  getBinds d _ = d

inBound :: forall n. (Ord n, Bounded n, Num n, Integral n) => Integer -> Bool
inBound i =
  (&&)
    (toInteger (minBound @n) >= i)
    (i <= toInteger (maxBound @n))

isValidLitNum :: NumRep -> Integer -> Bool
isValidLitNum NLitNumInteger _ = True
isValidLitNum NLitNumNatural i = i >= 0
isValidLitNum NLitNumBigNat i = i >= 0
isValidLitNum NLitNumInt i = (inBound @Int) i
isValidLitNum NLitNumInt8 i = (inBound @Int8) i
isValidLitNum NLitNumInt16 i = (inBound @Int16) i
isValidLitNum NLitNumInt32 i = (inBound @Int32) i
isValidLitNum NLitNumInt64 i = (inBound @Int64) i
isValidLitNum NLitNumWord i = (inBound @Word) i
isValidLitNum NLitNumWord8 i = (inBound @Word8) i
isValidLitNum NLitNumWord16 i = (inBound @Word16) i
isValidLitNum NLitNumWord32 i = (inBound @Word32) i
isValidLitNum NLitNumWord64 i = (inBound @Word64) i

-- end f NumRep

-- | Types of literals used in Core expressions.
data LiteralRep
  = -- | Character literal
    LChar Char
  | -- | Numeric literal with specific numeric type
    LNumber NumRep Integer
  | -- | String literal
    LString String
  | -- | Float literal
    LFloat Rational
  | -- | Double literal
    LDouble Rational
  | -- | Bottom (error) literal with a message
    Bottom String
  deriving (Show, Eq)

-- Put all functions to do with LiteralRep here:

isBottom :: LiteralRep -> Bool
isBottom (Bottom _) = True
isBottom _ = False

-- end f LiteralRep

data VarRep = VarRepped {varName :: String, varShortName :: String, varType :: TypeRep, varUnfolding :: Maybe ExprRep}

varUniquePrim :: VarRep -> String
varUniquePrim = (reverse . drop 5 . reverse . varName)

-- Put all functions to do with VarRep here:

instance Ord VarRep where
  -- ugly hack, but it's fine as long as varName stays unique
  compare v1 v2 = compare (varName v1) (varName v2)

instance Eq VarRep where
  (==) left right = (==) (varName left) (varName right)

instance Show VarRep where
  show = varName

instance CoreRep VarRep where
  alphaReduction x from (ExprVar to) = if x == from then to else x {varType = alphaReduction (varType x) from (ExprVar to)}
  alphaReduction x from to = x {varType = alphaReduction (varType x) from to}
  getBinds discovered i = case varUnfolding i of
    Nothing -> DS.insert i discovered
    Just internal -> lookIntoBreak discovered i (`getBinds` internal)
  getGraph x =
    Node
      "VarRep"
      [ ("name", Node (varName x) []),
        -- ("type", Node (drop 1 $ init $ show $ show $ varType x) []),
        -- ("type", getGraph (varType x)),
        ("hasUnfolding", Node (show (isJust $ varUnfolding x)) [])
      ]

-- end f VarRep

data TypeFunctionForm = TypeToType | TypeToCoercion | CoercionToType | CoercionToCoercion
  deriving (Eq)

instance Show TypeFunctionForm where
  show TypeToType = "->"
  show TypeToCoercion = "-=>"
  show CoercionToType = "=>"
  show CoercionToCoercion = "==>"

type TypeConRep = (String, [VarRep], [VarRep])

-- name, vars, constructors

data TypeRep
  = TypeVariable VarRep
  | TypeConstructor TypeConRep [TypeRep]
  | TypeFunction TypeRep TypeFunctionForm TypeRep
  | TypeForall VarRep TypeRep
  | TypeLiteral LiteralRep
  deriving (Eq)

-- Put all functions to do with TypeRep here:

instance Show TypeRep where
  show (TypeVariable v) = show v
  show (TypeConstructor (cs, ca, _) t) =
    foldr (\a acc -> "(" ++ acc ++ ") " ++ show a) (foldr (\a acc -> "\\" ++ show a ++ ". (" ++ acc ++ ")") (show cs) ca) t
  show (TypeFunction l f r) = "(" ++ show l ++ " " ++ show f ++ " " ++ show r ++ ")"
  show (TypeForall v t) = "(forall " ++ show v ++ ". " ++ show t ++ ")"
  show (TypeLiteral l) = show l

instance CoreRep TypeRep where
  alphaReduction (TypeVariable v) from (ExprType to)
    | v == from = to
    | otherwise = TypeVariable $ alphaReduction v from (ExprType to)
  alphaReduction (TypeVariable v) from to = TypeVariable $ alphaReduction v from to
  alphaReduction (TypeConstructor t app) from to = TypeConstructor t (map (\a -> alphaReduction a from to) app)
  alphaReduction (TypeFunction l f r) from to = TypeFunction (alphaReduction l from to) f (alphaReduction r from to)
  alphaReduction (TypeForall v t) from to
    | v == from = alphaReduction (alphaReduction t from to) v to
    | otherwise = TypeForall (alphaReduction v from to) (alphaReduction t from to)
  alphaReduction (TypeLiteral l) _ _ = TypeLiteral l
  getBinds = undefined
  getGraph (TypeVariable v) = Node "TypeVariable" [("var", getGraph v)]
  getGraph (TypeConstructor (s, binds, _) b) = Node "TypeConstructor" [("Con", Node s [("binds", getGraph binds)]), ("applied", getGraph b)]
  getGraph (TypeFunction a t r) = Node "TypeFunction" [("arg", getGraph a), ("type", Node (show t) []), ("res", getGraph r)]
  getGraph (TypeLiteral t) = Node "TypeLiteral" [("lit", getGraph t)]
  getGraph (TypeForall v t) = Node "TypeForall" [("forall", getGraph v), ("typeExp", getGraph t)]

-- end f TypeRep

data CoercionRep
  = CoercionUnknown
  | CoRefl TypeRep
  | CoGRefl TypeRep CoercionRep
  | CoForall VarRep CoercionRep CoercionRep
  | CoFun CoercionRep CoercionRep
  | CoSym CoercionRep
  | CoAxiomInst [CoercionRep]
  --  = CoRefl { coReflType :: TypeRep }
  --  | CoGRefl { coReflType :: TypeRep,  CoercionRep }
  --  | CoTyConAppCo TypeConRep
  --  | CoAppCo CoercionRep CoercionRep
  --  | CoForaAllCo VarRep CoercionRep CoercionRep
  --  | CoFunCo CoercionRep CoercionRep
  --  | CoVarCo VarRep
  --  | CoSymCo CoercionRep
  --  | CoTransCo CoercionRep CoercionRep
  deriving (Eq)

-- Put all functions to do with CoercionRep here:

instance Show CoercionRep where
  show (CoercionUnknown) = "CoercionUnknown"
  show (CoRefl t) = "CoRefl " ++ (reverse . drop 1 . reverse . drop 1 . show) t
  show (CoForall n b a) = "CoForall " ++ show n ++ ". (" ++ show b ++ ") " ++ show a
  show (CoFun l r) = "CoFun (" ++ show l ++ ") (" ++ show r ++ ")"
  show (CoSym arg) = "CoSym (" ++ show arg ++ ")"
  show (CoGRefl t c) = "CoGRefl (" ++ (reverse . drop 1 . reverse . drop 1 . show) t ++ ", " ++ show c ++ ")"
  show (CoAxiomInst cs) = "CoAxiomInst " ++ show cs

instance CoreRep CoercionRep where
  alphaReduction x from to = x
  getBinds = undefined
  getGraph CoercionUnknown = Node ("CoercionUnknown") []
  getGraph (CoRefl t) = Node "CoRefl" [("type", getGraph t)]
  getGraph (CoForall n b a) = Node "CoForall" [("bind", getGraph n), ("body", getGraph b), ("arg", getGraph a)]
  getGraph (CoFun l r) = Node "CoFun" [("l", getGraph l), ("r", getGraph r)]
  getGraph (CoSym arg) = Node "CoSym" [("arg", getGraph arg)]
  getGraph (CoGRefl t c) = Node "CoGRefl" [("type", getGraph t), ("coer", getGraph c)]
  getGraph (CoAxiomInst cs) = Node "CoAxiomInst" [("Coercions", getGraph cs)]

-- end f CoercionRep

-- Invariant: VarRep has no here unfolding
data Binding
  = BindNonRec VarRep ExprRep
  | BindRec [(VarRep, ExprRep)]
  deriving (Show, Eq)

-- Put all functions to do with Binding here:

instance CoreRep Binding where
  alphaReduction (BindNonRec v e) from to = BindNonRec v $ alphaReduction e from to
  alphaReduction (BindRec el) from to = BindRec $ map (\(v, e) -> (v, alphaReduction e from to)) el
  getBinds discovered (BindNonRec v e) = lookIntoBreak discovered v (`getBinds` e)
  getBinds discovered (BindRec el) = foldr foldf discovered el
    where
      foldf (v, e) acc = lookIntoBreak acc v (`getBinds` e)
  getGraph (BindNonRec v e) =
    Node
      "Bind"
      [ ("var", getGraph v),
        ("expr", getGraph e)
      ]
  getGraph (BindRec ve) =
    listNodesS "BindRec" $
      map
        ( \(v, e) ->
            Node
              "Bind"
              [ ("var", getGraph v),
                ("expr", getGraph e)
              ]
        )
        ve

-- end f Binding

type Program = [Binding]

data ExprRep
  = ExprVar VarRep
  | ExprLit LiteralRep
  | ExprApp ExprRep ExprRep
  | ExprLambda VarRep ExprRep
  | ExprLet Binding ExprRep
  | ExprCase ExprRep VarRep TypeRep [ExprAlt]
  | ExprCast ExprRep CoercionRep
  | ExprType TypeRep
  | ExprCoer CoercionRep
  deriving (Show, Eq)

data ExprAlt
  = Alt ExprCon [VarRep] ExprRep
  deriving (Show, Eq)

data ExprCon
  = ExprDataCon VarRep
  | ExprLitCon LiteralRep
  | DefaultCon
  deriving (Show, Eq)

-- Put all functions to do with ExprRep, ExprAlt or ExprCon here

isEqual :: ExprCon -> ExprRep -> Bool
isEqual (ExprDataCon v1) (ExprVar v2) = v1 == v2
isEqual (ExprLitCon l1) (ExprLit l2) = l1 == l2
isEqual _ _ = False

instance CoreRep ExprRep where
  alphaReduction (ExprVar v) from to
    | v == from = to
    | otherwise = ExprVar (alphaReduction v from to)
  alphaReduction (ExprLit l) _ _ = ExprLit l
  alphaReduction (ExprApp l r) from to = ExprApp (alphaReduction l from to) (alphaReduction r from to)
  alphaReduction (ExprLambda v e) from to = ExprLambda (alphaReduction v from to) (alphaReduction e from to)
  alphaReduction (ExprLet b e) from to = ExprLet b (alphaReduction e from to)
  alphaReduction (ExprCase m v t alt) from to =
    ExprCase
      (alphaReduction m from to)
      (alphaReduction v from to)
      (alphaReduction t from to)
      (alphaReduction alt from to)
  alphaReduction (ExprCast e c) from to = ExprCast (alphaReduction e from to) (alphaReduction c from to)
  alphaReduction (ExprType t) from to = ExprType (alphaReduction t from to)
  alphaReduction (ExprCoer c) from to = ExprCoer (alphaReduction c from to)

  getBinds = undefined
  getGraph (ExprVar v) = Node "ExprVar" [("var", getGraph v)]
  getGraph (ExprLit l) = Node "ExprLit" [("lit", getGraph l)]
  getGraph (ExprApp e a) = Node "ExprApp" [("expr", getGraph e), ("arg", getGraph a)]
  getGraph (ExprLambda v e) = Node "ExprLambda" [("var", getGraph v), ("expr", getGraph e)]
  getGraph (ExprLet b e) = Node "ExprLet" [("lets", getGraph b), ("expr", getGraph e)]
  getGraph (ExprCase m v t alt) = Node "ExprCase" [("expr", getGraph m), ("match", getGraph v), ("type", getGraph t), ("options", getGraph alt)]
  getGraph (ExprCast e c) = Node "ExprCast" [("expr", getGraph e), ("coercion", getGraph c)]
  getGraph (ExprType t) = Node "ExprType" [("type", getGraph t)]
  getGraph (ExprCoer c) = Node "ExprCoer" [("coercion", getGraph c)]

instance CoreRep ExprAlt where
  alphaReduction (Alt c b e) from to = Alt ca ba ea
    where
      ca = alphaReduction c from to
      ba = alphaReduction b from to
      ea = alphaReduction e from to

  -- c and b always nonletable vars (I think?)
  getBinds discovered (Alt _ _ e) = getBinds discovered e
  getGraph (Alt c b e) = Node "Alt" [("match", getGraph c), ("binders", getGraph b), ("expr", getGraph e)]

instance CoreRep ExprCon where
  alphaReduction (ExprDataCon d) from to = ExprDataCon $ alphaReduction d from to
  alphaReduction c _ _ = c
  getBinds discovered _ = discovered
  getGraph (ExprDataCon k) = getGraph k
  getGraph (ExprLitCon l) = Node (show l) []
  getGraph DefaultCon = Node "Default" []