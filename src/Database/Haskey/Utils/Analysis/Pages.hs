{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Haskey.Utils.Analysis.Pages where

import Control.Concurrent.STM
import Control.Lens (Lens', lens, (%~), (&))
import Control.Monad (void)
import Control.Monad.State (StateT, modify', execStateT)
import Control.Monad.Trans (lift)

import Data.Foldable (traverse_)
import Data.Traversable (traverse)

import Data.BTree.Alloc (AllocReaderM(..))
import Data.BTree.Impure (Tree(..))
import Data.BTree.Impure.NonEmpty (toTree)
import Data.BTree.Primitives (NodeId(..), PageId(..), nodeIdToPageId,
                              Height, decrHeight,
                              Key, Value)
import qualified Data.BTree.Impure as I

import Database.Haskey.Alloc.Concurrent (ConcurrentDb(..), transactReadOnly)
import Database.Haskey.Alloc.Concurrent.Environment (FreePage(..), getSValue)
import Database.Haskey.Alloc.Concurrent.FreePages.Tree (FreeTree)
import Database.Haskey.Alloc.Concurrent.Meta (ConcurrentMeta(..), CurrentMetaPage(..))
import Database.Haskey.Alloc.Concurrent.Overflow (OverflowTree)
import Database.Haskey.Store.File (runFileStoreT, defFileStoreConfig)

import Database.Haskey.Utils.Intervals hiding (Tree)

-- | Data type holding all 'PageId' we encountered (or saw).
data Saw = Saw {
    _sawDataPages :: Maybe (Collection PageId)
  , _sawDataTreePages :: Maybe (Collection PageId)
  , _sawDataFreePages :: Maybe (Collection PageId)
  , _sawDataFreeTreePages :: Maybe (Collection PageId)
  , _sawDataOverflowTreePages :: Maybe (Collection PageId)

  , _sawIndexPages :: Maybe (Collection PageId)
  , _sawIndexFreePages :: Maybe (Collection PageId)
  , _sawIndexTreePages :: Maybe (Collection PageId)
  , _sawIndexFreeTreePages :: Maybe (Collection PageId)
  , _sawIndexOverflowTreePages :: Maybe (Collection PageId)
  } deriving (Show)

-- | Print all encountered pages.
printSaw :: Saw -> String
printSaw Saw{..} = unlines [
    "sawDataPages: " ++ show' _sawDataPages
  , "sawDataTreePages: " ++ show' _sawDataTreePages
  , "sawDataFreePages: " ++ show' _sawDataFreePages
  , "sawDataFreeTreePages: " ++ show' _sawDataFreeTreePages
  , "sawDataOverflowTreePages: " ++ show' _sawDataOverflowTreePages
  , "sawIndexPages: " ++ show' _sawIndexPages
  , "sawIndexTreePages: " ++ show' _sawIndexTreePages
  , "sawIndexFreePages: " ++ show' _sawIndexFreePages
  , "sawIndexFreeTreePages: " ++ show' _sawIndexFreeTreePages
  , "sawIndexOverflowTreePages: " ++ show' _sawIndexOverflowTreePages
  ]
  where
   show' x = show (invertCollection <$> x)

defSaw :: Saw
defSaw = let c = Just (collection defInterval) in Saw c c c c c c c c c c

analyzeFileDatabase :: (Key k, Value v)
                => ConcurrentDb k v
                -> IO Saw
analyzeFileDatabase db = do
    meta <- atomically $ do
        m <- readTVar (concurrentDbCurrentMeta db)
        case m of Meta1 -> readTVar (concurrentDbMeta1 db)
                  Meta2 -> readTVar (concurrentDbMeta2 db)

    runDatabase $ transactReadOnly (analyzer meta) db
  where
    runDatabase action = runFileStoreT action defFileStoreConfig

    analyzer :: (AllocReaderM m, Key k, Value v)
             => ConcurrentMeta k v
             -> Tree k v
             -> m Saw
    analyzer meta _ = execStateT (analyzeMeta meta) defSaw

analyzeMeta :: (AllocReaderM m, Key k, Value v)
            => ConcurrentMeta k v
            -> StateT Saw m ()
analyzeMeta ConcurrentMeta{..} = do
    analyzeTree concurrentMetaTree
    analyzeFreeTree (getSValue concurrentMetaDataFreeTree)
        >>= \freePages -> do
            mapM_ (\p -> modify' (& sawDataPages     %~ (>>= puncture p))) freePages
            mapM_ (\p -> modify' (& sawDataFreePages %~ (>>= puncture p))) freePages
    analyzeFreeTree (getSValue concurrentMetaIndexFreeTree)
        >>= \freePages -> do
            mapM_ (\p -> modify' (& sawIndexPages     %~ (>>= puncture p))) freePages
            mapM_ (\p -> modify' (& sawIndexFreePages %~ (>>= puncture p))) freePages
    analyzeOverflowTree concurrentMetaOverflowTree

    traverse_ (\(FreePage p) -> modify' (& sawDataPages %~ (>>= puncture p))) $ getSValue concurrentMetaDataCachedFreePages
    traverse_ (\(FreePage p) -> modify' (& sawDataFreePages %~ (>>= puncture p))) $ getSValue concurrentMetaDataCachedFreePages

    traverse_ (\(FreePage p) -> modify' (& sawIndexPages %~ (>>= puncture p))) $ getSValue concurrentMetaIndexCachedFreePages
    traverse_ (\(FreePage p) -> modify' (& sawIndexFreePages %~ (>>= puncture p))) $ getSValue concurrentMetaIndexCachedFreePages

analyzeTree :: (AllocReaderM m, Key k, Value v)
            => Tree k v
            -> StateT Saw m ()
analyzeTree = analyzeTree' sawDataTreePages sawIndexTreePages

analyzeTree' :: (AllocReaderM m, Key k, Value v)
            => Lens' Saw (Maybe (Collection PageId))
            -> Lens' Saw (Maybe (Collection PageId))
            -> Tree k v
            -> StateT Saw m ()
analyzeTree' _ _ (Tree _ Nothing) = return ()
analyzeTree' dataSel indexSel (Tree h (Just nid)) =
    analyzeNode dataSel indexSel h nid

analyzeFreeTree :: AllocReaderM m
                => FreeTree
                -> StateT Saw m [PageId]
analyzeFreeTree tree = do
    analyzeTree' sawDataFreeTreePages sawIndexFreeTreePages tree
    subtrees <- lift $ map (toTree . snd) <$> I.toList tree
    void $ traverse (analyzeTree' sawDataFreeTreePages sawIndexFreeTreePages) subtrees
    lift $ concat <$> mapM (\t -> map fst <$> I.toList t) subtrees

analyzeOverflowTree :: AllocReaderM m
                    => OverflowTree
                    -> StateT Saw m ()
analyzeOverflowTree tree = do
    analyzeTree' sawDataOverflowTreePages sawIndexOverflowTreePages tree
    subtrees <- lift $ map (toTree . snd) <$> I.toList tree
    void $ traverse (analyzeTree' sawDataOverflowTreePages sawIndexOverflowTreePages) subtrees

analyzeNode :: (AllocReaderM m, Key k, Value v)
            => Lens' Saw (Maybe (Collection PageId))
            -> Lens' Saw (Maybe (Collection PageId))
            -> Height height
            -> NodeId height k v
            -> StateT Saw m ()
analyzeNode dataSel indexSel h nid = do
    n <- lift $ readNode h nid
    case n of
        I.Leaf _  -> do
            modify' (& sawDataPages %~ (>>= puncture' nid))
            modify' (& dataSel %~ (>>= puncture' nid))
        I.Idx idx -> do
            modify' (& sawIndexPages %~ (>>= puncture' nid))
            modify' (& indexSel %~ (>>= puncture' nid))
            void $ traverse (analyzeNode dataSel indexSel (decrHeight h)) idx

puncture' :: NodeId h k v -> Collection PageId -> Maybe (Collection PageId)
puncture' nid = puncture (nodeIdToPageId nid)

sawDataPages :: Lens' Saw (Maybe (Collection PageId))
sawDataPages = lens _sawDataPages $ \a x -> a { _sawDataPages = x }

sawDataFreeTreePages :: Lens' Saw (Maybe (Collection PageId))
sawDataFreeTreePages = lens _sawDataFreeTreePages $ \a x -> a { _sawDataFreeTreePages = x }

sawDataOverflowTreePages :: Lens' Saw (Maybe (Collection PageId))
sawDataOverflowTreePages = lens _sawDataOverflowTreePages $ \a x -> a { _sawDataOverflowTreePages = x }

sawDataTreePages :: Lens' Saw (Maybe (Collection PageId))
sawDataTreePages = lens _sawDataTreePages $ \a x -> a { _sawDataTreePages = x }

sawDataFreePages :: Lens' Saw (Maybe (Collection PageId))
sawDataFreePages = lens _sawDataFreePages $ \a x -> a { _sawDataFreePages = x }

sawIndexPages :: Lens' Saw (Maybe (Collection PageId))
sawIndexPages = lens _sawIndexPages $ \a x -> a { _sawIndexPages = x }

sawIndexFreePages :: Lens' Saw (Maybe (Collection PageId))
sawIndexFreePages = lens _sawIndexFreePages $ \a x -> a { _sawIndexFreePages = x }

sawIndexFreeTreePages :: Lens' Saw (Maybe (Collection PageId))
sawIndexFreeTreePages = lens _sawIndexFreeTreePages $ \a x -> a { _sawIndexFreeTreePages = x }

sawIndexOverflowTreePages :: Lens' Saw (Maybe (Collection PageId))
sawIndexOverflowTreePages = lens _sawIndexOverflowTreePages $ \a x -> a { _sawIndexOverflowTreePages = x }

sawIndexTreePages :: Lens' Saw (Maybe (Collection PageId))
sawIndexTreePages = lens _sawIndexTreePages $ \a x -> a { _sawIndexTreePages = x }
