{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines data types and instances for representing
-- Core language constructs and transformations used within GHC.
module CoreTranslate.Language where

import GHC.Core.TyCo.Compare (tcEqKind)
import GHC.Core.TyCo.Rep
import GHC.Plugins hiding (Case)
import Prelude

-- | Enumeration of numeric types in the Core language.
data NumTypes
  = -- | Integer type
    LitNumInteger
  | -- | Natural number type
    LitNumNatural
  | -- | Arbitrary precision natural numbers
    LitNumBigNat
  | -- | Standard Int type
    LitNumInt
  | -- | 8-bit Int type
    LitNumInt8
  | -- | 16-bit Int type
    LitNumInt16
  | -- | 32-bit Int type
    LitNumInt32
  | -- | 64-bit Int type
    LitNumInt64
  | -- | Word type (unsigned integer)
    LitNumWord
  | -- | 8-bit Word type
    LitNumWord8
  | -- | 16-bit Word type
    LitNumWord16
  | -- | 32-bit Word type
    LitNumWord32
  | -- | 64-bit Word type
    LitNumWord64
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
  | Dict
      DataCon
      -- | Dictionary literal representing type class instances
      [ProveExpression]
  | -- | Bottom (undefined) literal with a message
    Bottom String
  | -- | Constructor literal
    Constructor ProveName
  deriving (Eq)

-- | Custom 'Show' instance for 'LiteralTypes' for more detailed representation.
instance Show LiteralTypes where
  show (Dict d [])      = "{ dict = " ++ nameStableString (getName d) ++ " }"
  show (Dict d (x : _)) = "{ dict = " ++ nameStableString (getName d) ++ ", type = " ++ show x ++ " }"
  show (LChar c)        = "{ LChar = " ++ show c ++ " }"
  show (LNumber n i)    = "{ LNumber = " ++ show (n, i) ++ " }"
  show (LString s)      = "{ LString = " ++ show s ++ " }"
  show (LFloat r)       = "{ LFloat = " ++ show r ++ "}"
  show (LDouble r)      = "{ LDouble = " ++ show r ++ "}"
  show (Typed t)        = "{ Typed = " ++ show t ++ "}"
  show (Bottom e)       = "{ Bottom = " ++ show e ++ "}"
  show (Constructor v)  = "{ Constructor = " ++ show v ++ "}"

-- | Core language expressions, allowing various types of terms, variables, and constructs.
data ProveExpression
  = -- | Literal expression
    Literal {_lt :: LiteralTypes}
  | -- | Variable expression
    Variable {_name :: ProveName}
  | -- | Lambda expression
    Lambda {_name :: ProveName, _expr :: ProveExpression}
  | -- | Case expression with a binder and alternatives
    Case {_binding :: ProveExpression, _result_binder :: ProveName, _options :: [CaseInstance]}
  | -- | Direct operation between expressions
    DirectOperation {_expr :: ProveExpression, _arg :: ProveExpression}
  deriving (Show, Eq)

-- | Represents a single case alternative in a 'Case' expression.
data CaseInstance = CI AltCon [ProveName] ProveExpression
  deriving (Eq)

-- | Defines a variable and its associated expression in the Core language.
data VariableDef = Def {defName :: ProveName, defExpr :: ProveExpression}
  deriving (Show, Eq)

-- | Type synonym for the Core language, represented as a list of variable definitions.
type ProveLanguage = [VariableDef]

-- | Represents a variable name in the Core language.
newtype ProveName = PN Id

-- | Represents a type in the Core language.
newtype ProveType = PT Kind

-- | Custom 'Eq' instance for 'ProveName' based on its string representation.
instance Eq ProveName where
  a == b = show a == show b

-- | Generates a unique, stable string for a given 'ProveName'.
stableUnique :: ProveName -> String
stableUnique (PN v) = nameStableString (getName v)

-- | Custom 'Show' instance for 'DataCon' (data constructors) using their stable names.
instance Show DataCon where
  show d = nameStableString (getName d)

-- | Custom 'Show' instance for 'CaseInstance' for detailed representation.
instance Show CaseInstance where
  show (CI con bind caseExpr) = "constructor: " ++ showPprUnsafe con ++ ", binders: " ++ show bind ++ ", expression: " ++ show caseExpr

-- | Custom 'Show' instance for 'ProveName' using stable name and unique identifier.
instance Show ProveName where
  show (PN v) = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"

-- | Custom 'Show' instance for 'ProveType' for pretty-printing the kind.
instance Show ProveType where
  show (PT t) = showPprUnsafe t

-- | Custom 'Eq' instance for 'ProveType' using kind equality check.
instance Eq ProveType where
  (==) :: ProveType -> ProveType -> Bool
  (==) (PT kind1) (PT kind2) = tcEqKind kind1 kind2
