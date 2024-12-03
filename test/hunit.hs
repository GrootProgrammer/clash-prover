{-# LANGUAGE BlockArguments #-}
module Main where

import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import qualified Control.Monad
import qualified System.Exit

import qualified Tests.Language.LanguageUtils


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    _results <- sequence [
            Tests.Language.LanguageUtils.tests
        ]

    Control.Monad.unless (and _results) System.Exit.exitFailure