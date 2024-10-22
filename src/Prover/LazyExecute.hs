{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Prover.LazyExecute where

import CoreTranslate.Language
import Prelude hiding (lookup)

--type Conditionals = [ProveExpression]
--
--lookup :: ProveLanguage -> ProveName -> ProveType -> ProveExpression
--lookup dict name _ = defExpr $ head $ filter (\i -> defName i == name) dict 
--
--lazyExecution :: ProveLanguage -> ProveExpression -> Conditionals -> [ProveExpression]
--lazyExecution _     (Literal lt pt) cond = [Literal lt pt]
--lazyExecution _     (Symbolic s e pt) cond = [Symbolic s e pt]
--lazyExecution dict  (Variable s pt) cond = [lookup dict s pt]
--lazyExecution TypeVar        
--lazyExecution Lambda         
--lazyExecution Case           
--lazyExecution DirectOperation