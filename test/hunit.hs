{-# LANGUAGE BlockArguments #-}

module Main where

import Test.HUnit
import qualified Tests.Language.Expression

main :: IO ()
main = do
  counts <- runTestTT Tests.Language.Expression.tests
  return ()
