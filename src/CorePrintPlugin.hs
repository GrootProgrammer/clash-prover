{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module CorePrintPlugin (plugin) where

import Debug.Trace
import GHC.Plugins
import Language
import Properties
import Rewrite.Deconstruct
import Prelude
import Primitives

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

printSimplify :: ProveLanguage ProveName -> ProveExpression ProveName -> IO ()
printSimplify lang ex = putStr $ show $ iterateUntilNothing (traceWith (maybe "Nothing" (\e -> "starting graph:\n" ++ (showNode "" $ toGraphvizPath e) ++ "\nending graph\n")) . deconstructStep lang []) (Node ex)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  liftIO $ print lang
  mapM_ (liftIO . printSimplify lang . traceWith (\e -> "starting graph:\n" ++ (showNode "" $ toGraphviz e) ++ "\nending graph\n")) [left, right]
  return guts
  where
    (_, _, left, right) = firstEquiv
    firstEquiv = head equivs_partition
    equivs_partition = map (extractEquivInfo . defExpr) equivs
    equivs :: [VariableDef ProveName]
    equivs = findEquivInLanguage lang
    lang :: ProveLanguage ProveName
    lang = convertBinds $ mg_binds guts
