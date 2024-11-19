{-# LANGUAGE RankNTypes #-}

module CorePrintPlugin (plugin) where

import Debug.Trace
import GHC.Plugins
import Language
import Properties
import Rewrite.Simplify
import Prelude

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    }

getEither :: Either a a -> a
getEither (Left l) = l
getEither (Right r) = r

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = return (CoreDoPluginPass "Say name" pass : todo)

printSimplify :: ProveLanguage -> ProveExpression -> IO ()
printSimplify lang ex = putStr $ show $ iterateUntilLeft (traceWith (\e -> "starting graph:\n" ++ (toGraphviz (getEither e) `showNode` "") ++ "\nending graph\n") . simplifyStep lang [] []) ex

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  liftIO $ print lang
  mapM_ (liftIO . printSimplify lang . traceWith (\e -> "starting graph:\n" ++ (toGraphviz e `showNode` "") ++ "\nending graph\n") . defExpr) equivs
  return guts
  where
    equivs = findEquivInLanguage lang
    lang = convertBinds $ mg_binds guts
