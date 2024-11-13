module Execute.Primitives where

import CoreTranslate.Language
import CoreTranslate.LanguageUtils
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.TyCon
import qualified GHC.Types.Literal as TL
import GHC.Core

primitivesStr :: [String]
primitivesStr = [
        "ghc-prim$GHC.Prim$plusInt32#",
        "$ghc-prim$GHC.Prim$intToInt32#",
        "$ghc-prim$GHC.Prim$subInt32#",
        "$ghc-prim$GHC.Prim$wordToWord32#",
        "$ghc-bignum$GHC.Num.Integer$integerToWord#",
        "$ghc-bignum$GHC.Num.Integer$integerFromNatural",
        "$ghc-bignum$GHC.Num.Integer$IS",
        "$ghc-bignum$GHC.Num.Natural$NS",
        "$ghc-prim$GHC.Prim$tagToEnum#"
    ]

isPrimitive :: ProveExpression -> Bool
isPrimitive (Variable primName) = stableUnique primName `elem` primitivesStr
isPrimitive _ = False

getEnumFromType :: Type -> Maybe [DataCon]
getEnumFromType a =  tyConAppTyCon_maybe a >>= tyConDataCons_maybe

-- f stack e tries to resolve e if e is a primitive and enough stack values are correct signature, will reroll stack if resolved else return the input
primitiveExecute :: [ProveExpression] -> ProveExpression -> Either ProveExpression ProveExpression
primitiveExecute stack (Variable primName) = case (stack, stableUnique primName) of
    (Literal (LNumber LitNumInt32 an) : Literal (LNumber LitNumInt32 bn) : xs, "ghc-prim$GHC.Prim$plusInt32#")
        -> Right $ rerollStack xs $ Literal $ LNumber LitNumInt32 $ an + bn
    (Literal (LNumber LitNumInt an) : xs, "$ghc-prim$GHC.Prim$intToInt32#")
        -> Right $ rerollStack xs $ Literal $ LNumber LitNumInt32 an
    (Literal (LNumber LitNumInt32 an) : Literal (LNumber LitNumInt32 bn) : xs, "$ghc-prim$GHC.Prim$subInt32#")
        -> Right $ rerollStack xs $ Literal $ LNumber LitNumInt32 $ an - bn
    (Literal (LNumber LitNumWord an) : xs, "$ghc-prim$GHC.Prim$wordToWord32#")
        -> Right $ rerollStack xs $ Literal $ LNumber LitNumWord32 an
    ((Literal (LNumber LitNumInteger an)) : xs, "$ghc-bignum$GHC.Num.Integer$integerToWord#")
            -> Right $ rerollStack xs $ Literal (LNumber LitNumWord an)
    ((Literal (LNumber LitNumNatural an)) : xs, "$ghc-bignum$GHC.Num.Integer$integerFromNatural")
            -> Right $ rerollStack xs $ Literal (LNumber LitNumInteger an)
    ((Literal (LNumber LitNumInt an)) : xs, "$ghc-bignum$GHC.Num.Integer$IS")
            -> Right $ rerollStack xs $ Literal (LNumber LitNumInteger an)
    ((Literal (LNumber LitNumWord an)) : xs, "$ghc-bignum$GHC.Num.Natural$NS")
            -> Right $ rerollStack xs $ Literal (LNumber LitNumNatural an)
    ((Literal (Typed (PT t))) : p : xs, "$ghc-prim$GHC.Prim$tagToEnum#")
            -> case getEnumFromType t of
                Just e -> Right 
                            $ rerollStack xs 
                            $ CoreTranslate.Language.Case p primName
                            $ zipWith (\alt i -> CI (LitAlt $ TL.LitNumber TL.LitNumInt i) [] (Variable $ PN $ dataConWrapId alt)) e [0..]
                Nothing -> Left $ Variable primName
    (_ , _)
        -> Left (Variable primName)
primitiveExecute _ e = Left e
