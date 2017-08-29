{-# LANGUAGE RecordWildCards #-}
module Database.Haskey.Utils.Intervals.Draw where

import Data.Foldable (fold)

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine (B)

import Database.Haskey.Utils.Intervals

drawInterval :: Integral a => Interval a -> Diagram B
drawInterval Interval{..} = alignL $ square 1 # scaleX size'
  where
    val v | R v' <- v = v'
          | otherwise  = error "drawInterval: can't draw infinity"

    size' = fromIntegral $ val high - val low + 1

drawCollection :: Integral a => Collection a -> Diagram B
drawCollection c =
    let intervals = invertCollection c
        offsets   = map low' intervals
        segments  = map (\(i,o) -> drawInterval i # translateX o) $
                        zip intervals offsets
    in fold segments
  where
    low' (Interval (R l) _) = fromIntegral l
    low' _                  = error "drawCollection: infinity"
