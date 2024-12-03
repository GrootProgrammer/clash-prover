module Primitives
  ( isPrimitive,
    primitiveExecute,
    primitiveConsumption,
  )
where

import Debug.Trace (trace)
import GHC.Builtin.Types (falseDataConId)
import GHC.Plugins (trueDataConId)
import Language hiding (isPrimitive)
import Data.List (find)

--name, stackConsumption
primitivesStr ::
  [(String, Integer)]
primitivesStr =
  [ ("ghc-prim$GHC.Prim$plusInt32#",                    2),
    ("$ghc-prim$GHC.Prim$intToInt32#",                  1),
    ("$ghc-prim$GHC.Prim$subInt32#",                    2),
    ("$ghc-prim$GHC.Prim$wordToWord32#",                1),
    ("$ghc-bignum$GHC.Num.Integer$integerToWord#",      1),
    ("$ghc-bignum$GHC.Num.Integer$integerFromNatural",  1),
    ("$ghc-bignum$GHC.Num.Integer$IS",                  1),
    ("$ghc-bignum$GHC.Num.Natural$NS",                  1),
    ("$ghc-prim$GHC.Prim$tagToEnum#",                   1),
    ("$ghc-bignum$GHC.Num.Natural$naturalLt",           2),
    ("$ghc-bignum$GHC.Num.Natural$naturalLe",           2),
    ("$ghc-bignum$GHC.Num.Integer$integerToInt#",       1)
  ]

isPrimitive :: (LanguageName o) => ProveExpression o -> Bool
isPrimitive
  (Variable primName) =
    getShortName primName `elem` (map fst primitivesStr)
isPrimitive _ =
  False

primitiveConsumption :: (LanguageName o) => ProveExpression o -> Integer
primitiveConsumption
  (Variable primName) =
    case find (\(n,_) -> n == getShortName primName) primitivesStr of
      Nothing -> 0
      Just (_, i) -> i
primitiveConsumption _ = 0

-- f stack e tries to resolve e if e is a primitive and enough stack values are correct signature, will reroll stack if resolved else return the input
primitiveExecute :: (LanguageName l) => [ProveExpression l] -> ProveExpression l -> Either (ProveExpression l) (ProveExpression l)
primitiveExecute
  stack
  (Variable primName) =
    case (stack, getShortName primName) of
      ( Literal (LNumber NLitNumInt32 an) : Literal (LNumber NLitNumInt32 bn) : xs,
        "ghc-prim$GHC.Prim$plusInt32#"
        ) ->
          Right $
            rerollStack xs $
              Literal $
                LNumber NLitNumInt32 $
                  an + bn
      (Literal (LNumber NLitNumInt an) : xs, "$ghc-prim$GHC.Prim$intToInt32#") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumInt32 an
      (Literal (LNumber NLitNumInt32 an) : Literal (LNumber NLitNumInt32 bn) : xs, "$ghc-prim$GHC.Prim$subInt32#") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumInt32 $
                an - bn
      (Literal (LNumber NLitNumWord an) : xs, "$ghc-prim$GHC.Prim$wordToWord32#") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumWord32 an
      ((Literal (LNumber NLitNumInteger an)) : xs, "$ghc-bignum$GHC.Num.Integer$integerToWord#") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumWord an
      ((Literal (LNumber NLitNumNatural an)) : xs, "$ghc-bignum$GHC.Num.Integer$integerFromNatural") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumInteger an
      ((Literal (LNumber NLitNumInt an)) : xs, "$ghc-bignum$GHC.Num.Integer$IS") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumInteger an
      ((Literal (LNumber NLitNumWord an)) : xs, "$ghc-bignum$GHC.Num.Natural$NS") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumNatural an
      ((Literal (Typed t)) : p : xs, "$ghc-prim$GHC.Prim$tagToEnum#") ->
        case getConstructorFromType t of
          Just e ->
            Right $
              rerollStack xs $
                Case p primName $
                  zipWith
                    ( \alt i ->
                        createIntCI
                          i
                          []
                          (Variable alt)
                    )
                    e
                    [0 ..]
          Nothing ->
            trace
              ("Assumption broken: Could not get list of enum from: " ++ show p)
              Left
              $ Variable primName
      ((Literal (LNumber NLitNumNatural an)) : (Literal (LNumber NLitNumNatural ab)) : xs, "$ghc-bignum$GHC.Num.Natural$naturalLt") ->
        Right $
          rerollStack xs $
            Literal $
              Constructor
                (convert $ if an < ab then PN trueDataConId else PN falseDataConId)
      ((Literal (LNumber NLitNumNatural an)) : (Literal (LNumber NLitNumNatural ab)) : xs, "$ghc-bignum$GHC.Num.Natural$naturalLe") ->
        Right $
          rerollStack xs $
            Literal $
              Constructor
                (convert $ if an <= ab then PN trueDataConId else PN falseDataConId)
      ((Literal (LNumber NLitNumInteger an)) : xs, "$ghc-bignum$GHC.Num.Integer$integerToInt#") ->
        Right $
          rerollStack xs $
            Literal $
              LNumber NLitNumInt an
      (_, _) ->
        Left $
          Variable primName
primitiveExecute _ e = trace "no match" $ Left e
