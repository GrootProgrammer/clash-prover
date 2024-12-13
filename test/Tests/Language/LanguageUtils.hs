{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Language.LanguageUtils (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.LanguageUtils as Source
import Language.Types

-- alphaConversion testing
alphaConversion :: Property
alphaConversion =
  withTests 1 . property $ do
    _ <- (Source.alphaConversion (Variable (SON "a")) (SON "a") (Variable (SON "b"))) === (Variable (SON "b"))
    return ()

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Language.LanguageUtils" [
      ("alphaConversion", alphaConversion)
    ]