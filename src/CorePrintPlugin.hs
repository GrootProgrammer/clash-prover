module CorePrintPlugin (plugin) where
import GHC.Plugins
import CoreTranslate.Translate
import Data.List (intercalate)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
      liftIO $ putStrLn $ intercalate "\n" $ map show $ convertBinds $ mg_binds guts
      return guts