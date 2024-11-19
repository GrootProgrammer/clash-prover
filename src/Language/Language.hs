{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines data types and instances for representing
-- Core language constructs and transformations used within GHC.
module Language.Language
  ( NumTypes (..),
    LiteralTypes (..),
    ProveExpression (..),
    ProveLanguage,
    CaseInstance (..),
    VariableDef (..),
    ProveName (..),
    ProveType (..),
    CaseMatch (..),
    stableUnique,
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

-- | Types of literals used in Core expressions.
data LiteralTypes
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
    Constructor ProveName [ProveExpression]
  | Symbolic String
  deriving (Show, Eq)

-- | Core language expressions, allowing various types of terms, variables, and constructs.
data ProveExpression
  = -- | Literal expression
    Literal {_lt :: LiteralTypes}
  | -- | Variable expression
    Variable {_name :: ProveName}
  | -- | Lambda expression
    Lambda {_name :: ProveName, _expr :: ProveExpression}
  | -- | Let statement, equivalent to where and let (INVARIANT: only used in recursive calls)
    Let {letExpr :: VariableDef, _expr :: ProveExpression}
  | -- | Case expression with a binder and alternatives (INVARIANT: default is first case)
    Case {_binding :: ProveExpression, _result_binder :: ProveName, _options :: [CaseInstance]}
  | -- | Direct operation between expressions ( f a b -> DirectOperation (DirectOperation f a) b )
    DirectOperation {_expr :: ProveExpression, _arg :: ProveExpression}
  deriving (Show, Eq)

-- | Represents a single case alternative in a 'Case' expression.
data CaseInstance = CI CaseMatch [ProveName] ProveExpression
  deriving (Eq, Show)

-- | Represents the three possible cases of a case match, either an UNBOXED literal, a constructor or DEFAULT
data CaseMatch = CaseLit LiteralTypes | Cons ProveName | DEFAULT
  deriving (Eq, Show)

-- | Defines a variable and its associated expression in the Core language.
data VariableDef = Def {defName :: ProveName, defExpr :: ProveExpression}
  deriving (Show, Eq)

-- | Type synonym for the Core language, represented as a list of variable definitions.
type ProveLanguage = [VariableDef]

-- | Represents a variable name in the Core language.
newtype ProveName = PN GP.Id

-- | Represents a type in the Core language.
newtype ProveType = PT Kind

-- | Custom 'Eq' instance for 'ProveName' based on its string representation.
instance Eq ProveName where
  a == b = show a == show b

-- | Generates a unique, stable string for a given 'ProveName'.
stableUnique :: ProveName -> String
stableUnique (PN v) = GP.nameStableString (GP.getName v)

-- | Custom 'Show' instance for 'ProveName' using stable name and unique identifier.
instance Show ProveName where
  show (PN v) = GP.nameStableString (GP.getName v) ++ "(" ++ show (GP.getUnique $ GP.getName v) ++ ")"

-- | Custom 'Show' instance for 'ProveType' for pretty-printing the kind.
instance Show ProveType where
  show (PT t) = GP.showPprUnsafe t

-- | Custom 'Eq' instance for 'ProveType' using kind equality check.
instance Eq ProveType where
  (==) :: ProveType -> ProveType -> Bool
  (==) (PT kind1) (PT kind2) = tcEqKind kind1 kind2
