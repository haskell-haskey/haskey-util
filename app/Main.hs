module Main where

import Control.Applicative ((<$>))
import Control.Monad ((>=>), replicateM)

import Data.ByteString (ByteString, pack)

import Data.BTree.Impure (insertTree)

import Database.Haskey.Alloc.Concurrent (ConcurrentDb(..),
                                         ConcurrentHandles,
                                         concurrentHandles,
                                         createConcurrentDb,
                                         transact_,
                                         commit_)
import Database.Haskey.Store.File (FileStoreT, runFileStoreT, defFileStoreConfig)

import Diagrams.Backend.SVG.CmdLine (mainWith)

import System.Random (randomIO)

import Database.Haskey.Utils.Analysis.Pages
import Database.Haskey.Utils.Analysis.Pages.Draw

main :: IO ()
main = do
    let hnds = concurrentHandles "/tmp/test.haskey"
    db  <- createAndFill hnds
    saw <- analyzeFileDatabase db

    putStrLn $ printSaw saw
    mainWith $ drawSaw saw

createAndFill :: ConcurrentHandles
              -> IO (ConcurrentDb ByteString ByteString)
createAndFill hnds = do
    db    <- runDatabase $ createConcurrentDb hnds
    pairs <- genWrites
    mapM_ (tx db) pairs
    return db
  where
    tx db (k, v) = runDatabase $ transact_ (insertTree k v >=> commit_) db

runDatabase :: Monad m
            => FileStoreT FilePath m a
            -> m a
runDatabase action = runFileStoreT action defFileStoreConfig

genWrites :: IO [(ByteString, ByteString)]
genWrites = replicateM 10000 genPair
  where
    genPair = do
        key <- genKey
        val <- genVal
        return (key, val)

    genKey = do l <- (`rem` 20) <$> randomIO
                pack <$> replicateM l randomIO
    genVal = do l <- (`rem` 40) <$> randomIO
                pack <$> replicateM l randomIO
