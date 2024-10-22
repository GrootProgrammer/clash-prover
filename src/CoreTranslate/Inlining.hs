module CoreTranslate.Inlining where

import CoreTranslate.Language

inlineComplete :: ProveLanguage -> ProveExpression -> Bool
inlineComplete _ (Literal _ _) = True


inline :: ProveLanguage -> ProveExpression -> ProveExpression
inline 