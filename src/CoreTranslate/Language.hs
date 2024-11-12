
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CoreTranslate.Language where
import Prelude
import GHC.Core.TyCo.Rep
import GHC.Plugins hiding (Case)
import GHC.Core.TyCo.Compare (tcEqKind)

data NumTypes = LitNumInteger | LitNumNatural | LitNumBigNat | LitNumInt | LitNumInt8 | LitNumInt16 | LitNumInt32 | LitNumInt64 | LitNumWord | LitNumWord8 | LitNumWord16 | LitNumWord32 | LitNumWord64
    deriving (Show, Eq)

data LiteralTypes = LChar Char | LNumber NumTypes Integer | LString String | LFloat Rational | LDouble Rational | Typed ProveType | Dict DataCon [ProveExpression] | Bottom String | Constructor ProveName
    deriving (Eq)

instance Show LiteralTypes where
    show (Dict d [])    = "{ dict = " ++ nameStableString (getName d) ++ " }"
    show (Dict d (x:_)) = "{ dict = " ++ nameStableString (getName d) ++ ", type = " ++ show x ++ " }"
    show (LChar c)      = "{ LChar = " ++ show c ++ " }"
    show (LNumber n i)  = "{ LNumber = " ++ show (n, i) ++ " }"
    show (LString s)    = "{ LString = " ++ show s ++ " }"
    show (LFloat r)     = "{ LFloat = " ++ show r ++ "}"
    show (LDouble r)    = "{ LFloat = " ++ show r ++ "}"
    show (Typed t)      = "{ Typed = " ++ show t ++ "}"
    show (Bottom e)     = "{ Bottom = " ++ show e ++ "}"
    show (Constructor v)  = "{ Constructor = " ++ show v ++ "}"


data ProveExpression    = Literal           { _lt :: LiteralTypes}
                        | Variable          { _name :: ProveName}
                        | Lambda            { _name :: ProveName, _expr :: ProveExpression }
                        | Case              { _binding :: ProveExpression, _result_binder :: ProveName, _options :: [CaseInstance] }
                        | DirectOperation   { _expr :: ProveExpression, _arg :: ProveExpression }
    deriving (Show, Eq)

data CaseInstance   = CI AltCon [ProveName] ProveExpression
    deriving (Eq)

data VariableDef = Def { defName :: ProveName, defExpr :: ProveExpression}
    deriving (Show, Eq)

type ProveLanguage  = [VariableDef]

newtype ProveName = PN Id
newtype ProveType = PT Kind

instance Eq ProveName where
    a == b = show a == show b 

stableUnique :: ProveName -> String
stableUnique (PN v) = nameStableString (getName v)

instance Show DataCon where
    show d = nameStableString (getName d)

instance Show CaseInstance where
    show (CI con bind caseExpr) = "constructor: " ++ showPprUnsafe con ++ ", binders: " ++ show bind ++ ", expression: " ++ show caseExpr

instance Show ProveName where
    show (PN v) = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"
instance Show ProveType where
    show (PT t) = showPprUnsafe t
instance Eq ProveType where
  (==) :: ProveType -> ProveType -> Bool
  (==) (PT kind1) (PT kind2) = tcEqKind kind1 kind2
