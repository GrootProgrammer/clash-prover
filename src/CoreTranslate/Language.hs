{-# LANGUAGE InstanceSigs #-}
module CoreTranslate.Language where
import Data.Kind (Type)
import Data.Data (TypeRep)
import GHC (Kind)
import GHC.Core.TyCo.Rep
import Data.List (intercalate)
import qualified GHC.Real

data LiteralTypes = LChar Char | LNumber Integer | LString String | LFloat Rational | LDouble Rational | Typed Kind

instance Show LiteralTypes where
    show (LChar c) = "LChar '" ++ show c ++ "'"
    show (LNumber n) = "LNumber '" ++ show n ++ "'"
    show (LString s) = "LString '" ++ s ++ "'"
    show (LFloat r) = "LFloat '" ++ show r ++ "'"
    show (LDouble r) = "LDouble '" ++ show r ++ "'"
    show (Typed _) = "Typed"

data ProveExpression    = Literal LiteralTypes Kind
                        | Symbolic ProveExpression Kind
                        | Variable String Kind
                        | TypeVar Kind
                        | Lambda String ProveExpression Kind
                        | Case [CaseInstance] Kind
                        | DirectOperation ProveExpression ProveExpression Kind

instance Show ProveExpression where
    show :: ProveExpression -> String
    show (Literal l _) = "Literal: (" ++ show l ++ ")" 
    show (Symbolic l _) = "Symbolic: (" ++ show l ++ ")" 
    show (Variable l _) = "Variable: (" ++ show l ++ ")"
    show (TypeVar _) = show "TypeInfo"
    show (Lambda s l _) = "(\\" ++ show s ++ " -> " ++ show l ++ ")" 
    show (Case l _) = "Cases: [" ++ show l ++ "]"
    show (DirectOperation f arg _) = "DirectOperation: (" ++ show f ++ ") (" ++ show arg ++ ")"

data CaseInstance   = CI ProveExpression ProveExpression
    deriving (Show)

data VariableDef = Def { defName :: String, defExpr :: ProveExpression}
    deriving (Show)

type ProveLanguage  = [VariableDef]