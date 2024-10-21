{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CoreTranslate.Language where
import Data.Kind (Type)
import Data.Data (TypeRep)
import GHC (Kind, Id)
import GHC.Core.TyCo.Rep
import Data.List (intercalate)
import qualified GHC.Real
import GHC.Plugins hiding (Case)

data LiteralTypes = LChar Char | LNumber Integer | LString String | LFloat Rational | LDouble Rational | Typed ProveType
    deriving (Show)

data ProveExpression    = Literal LiteralTypes ProveType
                        | Symbolic ProveExpression ProveType
                        | Variable ProveName ProveType
                        | TypeVar ProveType
                        | Lambda ProveName ProveExpression ProveType
                        | Case [CaseInstance] ProveType
                        | DirectOperation ProveExpression ProveExpression ProveType
    deriving (Show)

data CaseInstance   = CI ProveExpression ProveExpression
    deriving (Show)

data VariableDef = Def { defName :: ProveName, defExpr :: ProveExpression}
    deriving (Show)

type ProveLanguage  = [VariableDef]

newtype ProveName = PN Id
newtype ProveType = PT Kind

instance Show ProveName where
    show (PN v) = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"
instance Show ProveType where
    show _ = "TypeInfo"

--getUndefinedVariables :: ProveLanguage -> [(String, Kind)]
--getUndefinedVariables [] = []
--getUndefinedVariables (x:xs) = concat (getUndefinedVariablesExpr x) (getUndefinedVariables xs)
--
--getUndefinedVariablesExpr :: VariableDef -> [(String)]