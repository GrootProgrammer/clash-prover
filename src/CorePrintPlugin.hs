{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module CorePrintPlugin (plugin) where

import Prelude
import GHC.Plugins
import CoreTranslate.Translate
import Data.List (intercalate)
import Properties (findEquivInLanguage)
import CoreTranslate.Language
import GHC (GhcMonad(..), LoadHowMuch (LoadAllTargets))
import GHC.Utils.Exception (ExceptionMonad)
import GHC.Driver.Make
import GHC.Driver.Monad
import Control.Monad.Catch

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

type CoreGHC = Ghc (CoreM ())

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  --d <- CoreTranslate.Unfolding.lookup n_1
  liftIO $ print $ (show n_1)
  return guts
      where
        ghcInstance = liftGhcT @CoreM
        (Variable n_1 _, equiv_1_2) = head equivs
        equivs = findEquivInLanguage lang
        lang = convertBinds $ mg_binds guts