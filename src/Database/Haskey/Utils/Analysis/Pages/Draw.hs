{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Haskey.Utils.Analysis.Pages.Draw where

import Data.Maybe (fromMaybe)

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine (B)

import Graphics.SVGFonts (textSVG)

import Data.BTree.Primitives (PageId(..))

import Database.Haskey.Utils.Analysis.Pages
import Database.Haskey.Utils.Intervals.Draw

drawSaw :: Saw -> Diagram B
drawSaw Saw{..} =
    vcat $ [alignL (stroke (textSVG "Data File" 2) # fc black # lw none)]
         ++ drawConfig dataConfig
         ++ [alignL (stroke (textSVG "Index File" 2) # fc black # lw none)]
         ++ drawConfig indexConfig
         ++ [alignL (stroke (textSVG "Legend" 2) # fc black # lw none)]
         ++ map drawDescr descrs
  where
    drawConfig = map drawPages

    drawPages (n, col, attrs) =
        let label :: Diagram B
            label = stroke (textSVG n 1) # fc black # lw none
        in
        alignR label <> fromMaybe mempty (attrs . drawCollection <$> col)

    dataConfig = [
        ("all"          , _sawDataPages            , fc black  . lw none)
      , ("tree"         , _sawDataTreePages        , fc blue . lw none)
      , ("free"         , _sawDataFreePages        , fc green  . lw none)
      , ("free tree"    , _sawDataFreeTreePages    , fc red    . lw none)
      , ("overflow tree", _sawDataOverflowTreePages, fc orange . lw none)
      ]

    indexConfig = [
        ("all"          , _sawIndexPages            , fc black  . lw none)
      , ("tree"         , _sawIndexTreePages        , fc blue   . lw none)
      , ("free"         , _sawIndexFreePages        , fc green  . lw none)
      , ("free tree"    , _sawIndexFreeTreePages    , fc red    . lw none)
      , ("overflow tree", _sawIndexOverflowTreePages, fc orange . lw none)
      ]


    drawDescr :: (String, String) -> Diagram B
    drawDescr (x, y) =
             alignR (stroke (textSVG (x ++ ":") 1) # fc black # lw none)
        <> (alignL (stroke (textSVG y 1) # fc black # lw none) # translateX 1)
    descrs = [("all", "Reachable pages in the file"),
              ("tree", "Pages used to store actual data"),
              ("free", "Free pages, ready for reuse"),
              ("free tree", "Pages belonging to the free tree, used "
                         ++ "to store free pages."),
              ("overflow tree", "Pages belonging to the overflow tree")]

deriving instance Enum PageId
deriving instance Real PageId
deriving instance Integral PageId
