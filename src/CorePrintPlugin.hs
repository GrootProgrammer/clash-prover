{-# LANGUAGE RankNTypes #-}
module CorePrintPlugin (plugin) where

import Prelude
import GHC.Plugins
import CoreTranslate.Translate
import Properties
import CoreTranslate.Language
import CoreTranslate.LanguageUtils
import Execute.Simplify
import Debug.Trace

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

getEither :: Either a a -> a
getEither (Left l) = l
getEither (Right r) = r

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = return (CoreDoPluginPass "Say name" pass : todo)

printSimplify :: ProveLanguage -> ProveExpression -> IO ()
printSimplify lang ex = putStr $ show $ iterateUntilLeft (traceWith (\e -> "starting graph:\n" ++ (tographviz (getEither e) `showNode` "") ++ "\nending graph\n") . simplifyStep lang [] []) ex

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  liftIO $ print lang
--  mapM_ (liftIO . printSimplify lang . traceWith (\e -> "starting graph:\n" ++ (tographviz e `showNode` "") ++ "\nending graph\n") . defExpr) equivsMealy
  mapM_ (liftIO . printSimplify lang . traceWith (\e -> "starting graph:\n" ++ (tographviz e `showNode` "") ++ "\nending graph\n") . defExpr) equivs
  return guts
      where
        equivs = findEquivInLanguage lang
        equivsMealy = findEquivMealyInLanguage lang
        lang = convertBinds $ mg_binds guts