{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module CorePrintPlugin (plugin) where

import Data.Data
import Data.Maybe (catMaybes, isJust)
import GHC.Plugins
import Language.Expression
import Language.Translate (convertBinds, convertExpr)
import Rewrite.OperationalRewrite (applyOpRules)
import Utils.ToGraphviz (showNode)
import Prelude

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = return (CoreDoPluginPass "Say name" pass : todo)

-- stolen from https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html
annotationsOn :: (Data a) => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  (_, anns) <- getAnnotations deserializeWithData guts
  return $ lookupWithDefaultUFM_Directly anns [] (varUnique bndr)

getBindsTuple :: [CoreBind] -> [(CoreBndr, CoreExpr)]
getBindsTuple ((NonRec b e) : xs) = (b, e) : getBindsTuple xs
getBindsTuple ((Rec ((b, e) : rs)) : xs) = (b, e) : getBindsTuple (Rec rs : xs)
getBindsTuple ((Rec []) : xs) = getBindsTuple xs
getBindsTuple [] = []

hasStringAnn :: ModGuts -> String -> CoreBndr -> CoreExpr -> CoreM (Maybe (String, ExprRep))
hasStringAnn guts match bind expr = do
  stringAnns <- (annotationsOn @String) guts bind
  return if match `elem` stringAnns then Just (nameStableString $ GHC.Plugins.varName bind, convertExpr expr) else Nothing

getAlwaysTrue :: ModGuts -> CoreM [(String, ExprRep)]
getAlwaysTrue guts = do
  maybeAlways <- mapM (\(b, e) -> hasStringAnn guts "alwaysTrue" b e) allBinds
  return $ catMaybes maybeAlways
  where
    allBinds = getBindsTuple $ mg_binds guts

solveAlwaysTrue :: String -> ExprRep -> IO ()
solveAlwaysTrue name expr = do
  putStrLn ("solving: " ++ name)
  putStrLn "starting graph:"
  putStr (showNode "" $ getGraph expr)
  putStrLn "ending graph"
  reworked <- pure $ applyOpRules expr
  liftIO (maybe (pure ()) (solveAlwaysTrue name) reworked)
  return ()

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  --  liftIO $ print lang
  liftIO $ putStr (showPprUnsafe $ mg_binds guts)
  alwaysTrue <- getAlwaysTrue guts
  --  liftIO $ print $ map fst alwaysTrue
  liftIO $ mapM_ (uncurry solveAlwaysTrue) alwaysTrue
  -- liftIO $ putStr $ showNode "" $ getGraph lang
  return guts
  where
    -- (_, _, left, right) = firstEquiv
    -- firstEquiv = head equivs_partition
    -- equivs_partition = map (extractEquivInfo . defExpr) equivs
    -- equivs = findEquivInLanguage lang
    lang = convertBinds $ mg_binds guts
