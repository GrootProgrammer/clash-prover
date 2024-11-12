{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Prove.ProveSimpleEquiv where

import CoreTranslate.Language
import Execute.Simplify (simplify)

proveSimpleEquiv :: ProveLanguage -> (ProveExpression, ProveExpression) -> IO (ProveExpression, ProveExpression)
proveSimpleEquiv lang (a,b) = pure (simp_a, simp_b)
    where
        simp_a = simplify lang [] a
        simp_b = simplify lang [] b