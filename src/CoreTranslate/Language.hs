
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
module CoreTranslate.Language where
import GHC.Core.TyCo.Rep
import GHC.Plugins hiding (Case)
import GHC.Core.TyCo.Compare (tcEqKind)

data LiteralTypes = LChar Char | LNumber Integer | LString String | LFloat Rational | LDouble Rational | Typed ProveType
    deriving (Show, Eq)

data ProveExpression    = Literal           { lt :: LiteralTypes, pt :: ProveType}
                        | Variable          { name :: ProveName, pt :: ProveType }
                        | TypeVar           { pt :: ProveType }
                        | Lambda            { name :: ProveName, expr :: ProveExpression, pt :: ProveType }
                        | Case              { options :: [CaseInstance], pt :: ProveType }
                        | DirectOperation   { f :: ProveExpression, arg :: ProveExpression, pt :: ProveType }
    deriving (Show, Eq)

data CaseInstance   = CI ProveExpression ProveExpression
    deriving (Show, Eq)

data VariableDef = Def { defName :: ProveName, defExpr :: ProveExpression}
    deriving (Show, Eq)

type ProveLanguage  = [VariableDef]

newtype ProveName = PN Id
    deriving (Eq)
newtype ProveType = PT Kind

instance Show ProveName where
    show (PN v) = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"
instance Show ProveType where
    show _ = "TypeInfo"
instance Eq ProveType where
  (==) :: ProveType -> ProveType -> Bool
  (==) (PT kind1) (PT kind2) = tcEqKind kind1 kind2
