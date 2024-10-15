{-# LANGUAGE DeriveDataTypeable #-}
module CorePrintPlugin (plugin) where
import GHC.Plugins
import CorePrinter

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
      mapM_ printCode (mg_binds guts)
      return guts

