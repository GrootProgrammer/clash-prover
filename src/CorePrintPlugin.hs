{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module CorePrintPlugin (plugin) where

import Prelude
import GHC.Plugins
import CoreTranslate.Translate
import Properties (findEquivInLanguage)
import CoreTranslate.Language

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  --d <- CoreTranslate.Unfolding.lookup n_1
  liftIO $ print (convertExpression $ unfoldingTemplate $ realIdUnfolding  n_1)
  return guts
      where
        (Variable (PN n_1), equiv_1_2) = head equivs
        equivs = findEquivInLanguage lang
        lang = convertBinds $ mg_binds guts