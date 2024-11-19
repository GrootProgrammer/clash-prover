module Primitives
  ( isPrimitive,
    primitiveExecute,
  )
where

import Debug.Trace (trace)
import GHC.Builtin.Types (falseDataConId)
import GHC.Plugins (trueDataConId)
import Language hiding (isPrimitive)
import Language.Language (ProveName (PN))

primitivesStr ::
  [String]
primitivesStr =
  [ "ghc-prim$GHC.Prim$plusInt32#",
    "$ghc-prim$GHC.Prim$intToInt32#",
    "$ghc-prim$GHC.Prim$subInt32#",
    "$ghc-prim$GHC.Prim$wordToWord32#",
    "$ghc-bignum$GHC.Num.Integer$integerToWord#",
    "$ghc-bignum$GHC.Num.Integer$integerFromNatural",
    "$ghc-bignum$GHC.Num.Integer$IS",
    "$ghc-bignum$GHC.Num.Natural$NS",
    "$ghc-prim$GHC.Prim$tagToEnum#",
    "$ghc-bignum$GHC.Num.Natural$naturalLt",
    "$ghc-bignum$GHC.Num.Natural$naturalLe"
  ]

isPrimitive ::
  ProveExpression ->
  Bool
isPrimitive
  (Variable primName) =
    stableUnique primName `elem` primitivesStr
isPrimitive _ =
  False

-- f stack e tries to resolve e if e is a primitive and enough stack values are correct signature, will reroll stack if resolved else return the input
primitiveExecute ::
  [ProveExpression] ->
  ProveExpression ->
  Either ProveExpression ProveExpression
primitiveExecute
  stack
  (Variable primName) =
    case (stack, stableUnique primName) of
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
                (if an < ab then PN trueDataConId else PN falseDataConId)
                []
      ((Literal (LNumber NLitNumNatural an)) : (Literal (LNumber NLitNumNatural ab)) : xs, "$ghc-bignum$GHC.Num.Natural$naturalLe") ->
        Right $
          rerollStack xs $
            Literal $
              Constructor
                (if an <= ab then PN trueDataConId else PN falseDataConId)
                []
      (_, _) ->
        Left $
          Variable primName
primitiveExecute _ e = Left e
