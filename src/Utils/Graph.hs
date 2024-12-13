{-# LANGUAGE FlexibleInstances #-}
module Utils.Graph where

import GHC.Utils.Outputable

-- | represents a node in the tree for Graphviz
data Graph = Node String [(String, Graph)]

instance Outputable Graph where
    ppr (Node name children) =
        text name GHC.Utils.Outputable.<> lbrace $$
        nest 1 (vcat (map formatChild children)) $$
        rbrace
      where
        formatChild (label, graph) =
            text label GHC.Utils.Outputable.<> colon <+> lbrace $$
            nest 1 (ppr graph) $$
            rbrace