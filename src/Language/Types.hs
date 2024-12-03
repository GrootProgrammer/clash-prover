{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines data types and instances for representing
-- Core language constructs and transformations used within GHC.

module Language.Types
  ( NumTypes (..),
    LanguageName (..),
    LiteralTypes (..),
    ProveExpression (..),
    ProveLanguage,
    CaseInstance (..),
    CaseMatch (..),
    VariableDef (..),
    ProveName (..),
    ProveType (..),
    StringName (..),
    StringOnlyName(..),
  )
where

import GHC.Core.TyCo.Compare (tcEqKind)
import GHC.Core.TyCo.Rep
import qualified GHC.Plugins as GP
import Prelude

-- | Enumeration of numeric types in the Core language.
data NumTypes
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

-- | A workaround to the fact that we cannot generate Id *easily* while already in the plugin
class (Show o, Eq o) => LanguageName o where
  getName :: o -> String
  getShortName :: o -> String
  getShortName = getName
  getType :: o -> ProveType
  updateType :: o -> ProveType -> o
  convert :: ProveName -> o
  convert n = create (getName n) (getType n)
  create :: String -> ProveType -> o
  getProveName :: o -> ProveName
  getProveName = undefined

-- | Types of literals used in Core expressions.
data (LanguageName n) => LiteralTypes n
  = -- | Character literal
    LChar Char
  | -- | Numeric literal with specific numeric type
    LNumber NumTypes Integer
  | -- | String literal
    LString String
  | -- | Float literal
    LFloat Rational
  | -- | Double literal
    LDouble Rational
  | -- | Typed literal
    Typed ProveType
  | -- | Bottom (error) literal with a message
    Bottom String
  | -- | Constructor literal
    Constructor n
  | Symbolic (SymbolicTree n)
  | Coercion
  deriving (Eq)

data PrimType = PrimChar | PrimNumber NumTypes | PrimString | PrimFloat | PrimDouble
  deriving (Show, Eq)

data (LanguageName n) => SymbolicTree n
  = NodePrim PrimType
  | Node (LiteralTypes n)
  | Add (SymbolicTree n) (SymbolicTree n)
  | Sub (SymbolicTree n) (SymbolicTree n)
  | Cast PrimType (SymbolicTree n)
  | Eq (SymbolicTree n) (SymbolicTree n)
  | Gt (SymbolicTree n) (SymbolicTree n)
  deriving (Show, Eq)

instance (Show n, LanguageName n) => Show (LiteralTypes n) where
  show (LChar c) = "LChar " ++ show c
  show (LNumber numType i) = "LNumber " ++ show numType ++ " " ++ show i
  show (LString s) = "LString " ++ show s
  show (LFloat r) = "LFloat " ++ show r
  show (LDouble r) = "LDouble " ++ show r
  show (Typed proveType) = "Typed " ++ show proveType
  show (Bottom _) = "_|_"
  show (Constructor n) = "Constructor " ++ show n
  show (Symbolic s) = "Symbolic " ++ show s
  show Coercion = "Coercion "

-- | Core language expressions, allowing various types of terms, variables, and constructs.
data (LanguageName l) => ProveExpression l
  = -- | Literal expression
    Literal {_lt :: LiteralTypes l}
  | -- | Variable expression
    Variable {_name :: l}
  | -- | Lambda expression
    Lambda {_name :: l, _expr :: ProveExpression l}
  | -- | Let statement, equivalent to where and let (INVARIANT: only used in recursive calls)
    Let {letExpr :: VariableDef l, _expr :: ProveExpression l}
  | -- | Case expression with a binder and alternatives (INVARIANT: default is first case)
    Case {_binding :: ProveExpression l, _result_binder :: l, _options :: [CaseInstance l]}
  | -- | Direct operation between expressions ( f a b -> DirectOperation (DirectOperation f a) b )
    DirectOperation {_expr :: ProveExpression l, _arg :: ProveExpression l}
  deriving (Show, Eq)

-- | Represents a single case alternative in a 'Case' expression.
data (LanguageName l) => CaseInstance l = CI (CaseMatch l) [l] (ProveExpression l)
  deriving (Eq, Show)

-- | Represents the three possible cases of a case match, either an UNBOXED literal, a constructor or DEFAULT
data (LanguageName l) => CaseMatch l = CaseLit (LiteralTypes l) | Cons l | DEFAULT
  deriving (Eq, Show)

-- | Defines a variable and its associated expression in the Core language.
data (LanguageName l) => VariableDef l = Def {defName :: l, defExpr :: ProveExpression l}
  deriving (Show, Eq)

-- | Type synonym for the Core language, represented as a list of variable definitions.
type ProveLanguage l = [VariableDef l]

-- | Represents a variable name in the Core language.
newtype ProveName = PN GP.Id

-- | Custom 'Eq' instance for 'ProveName' based on its string representation.
instance Eq ProveName where
  a == b = show a == show b

-- | Generates a unique, stable string for a given 'ProveName'.
stableUnique :: ProveName -> String
stableUnique (PN v) = GP.nameStableString (GP.getName v)

-- | Custom 'Show' instance for 'ProveName' using stable name and unique identifier.
instance Show ProveName where
  show (PN v) = GP.nameStableString (GP.getName v) ++ "(" ++ show (GP.getUnique $ GP.getName v) ++ ")"

instance LanguageName ProveName where
  getName = show
  getShortName = stableUnique
  getType (PN n) = PT $ GP.varType n
  convert = id
  updateType (PN n) (PT t) = PN $ GP.setVarType n t
  create = undefined
  getProveName = id

data StringName = SN {_getName :: String, _getType :: ProveType}
  deriving (Show, Eq)

instance LanguageName StringName where
  getName = _getName
  getType = _getType
  updateType (SN name _) = SN name
  create = SN


newtype StringOnlyName = SON {_getNameSON :: String}
  deriving (Show, Eq)

instance LanguageName StringOnlyName where
  getName = _getNameSON
  getType = undefined
  updateType i _ = i
  create s _ = SON s 

type StringOrCoreName = Either ProveName StringName

instance LanguageName StringOrCoreName where
  getName = either getName getName
  getShortName = either getShortName getShortName
  getType = either getType getType
  updateType lr t = either (\v -> Left $ updateType v t) (\v -> Right $ updateType v t) lr
  create = (Right .) . create
  getProveName = either getProveName getProveName

-- | Represents a type in the Core language.
newtype ProveType = PT {_getKind :: Kind}

-- | Custom 'Show' instance for 'ProveType' for pretty-printing the kind.
instance Show ProveType where
  show (PT t) = GP.showPprUnsafe t

-- | Custom 'Eq' instance for 'ProveType' using kind equality check.
instance Eq ProveType where
  (==) :: ProveType -> ProveType -> Bool
  (==) (PT kind1) (PT kind2) = tcEqKind kind1 kind2
