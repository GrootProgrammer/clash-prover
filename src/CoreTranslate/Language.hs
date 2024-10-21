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

data LiteralTypes = LChar Char | LNumber Integer | LString String | LFloat Rational | LDouble Rational | Typed Kind
    deriving (Show)

data ProveExpression    = Literal LiteralTypes Kind
                        | Symbolic ProveExpression Kind
                        | Variable Id Kind
                        | TypeVar Kind
                        | Lambda Id ProveExpression Kind
                        | Case [CaseInstance] Kind
                        | DirectOperation ProveExpression ProveExpression Kind
    deriving (Show)

data CaseInstance   = CI ProveExpression ProveExpression
    deriving (Show)

data VariableDef = Def { defName :: Id, defExpr :: ProveExpression}
    deriving (Show)

type ProveLanguage  = [VariableDef]

instance Show Id where
    show v = nameStableString (getName v) ++ "(" ++ show (getUnique $ getName v) ++ ")"
instance Show Kind where
    show v = "TypeInfo"

--getUndefinedVariables :: ProveLanguage -> [(String, Kind)]
--getUndefinedVariables [] = []
--getUndefinedVariables (x:xs) = concat (getUndefinedVariablesExpr x) (getUndefinedVariables xs)
--
--getUndefinedVariablesExpr :: VariableDef -> [(String)]