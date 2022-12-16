module DevelMain where

import           Application              (getApplicationRepl, shutdownApp)
import           Prelude

import           Control.Concurrent
import           Control.Monad            ((>=>))
import           Data.IORef
import           Foreign.Store
import           GHC.Word
import           Network.Wai.Handler.Warp

update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start


    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start done = do
        (port, site, app) <- getApplicationRepl
        forkFinally
            (runSettings (setPort port defaultSettings) app)
            (\_ -> putMVar done () >> shutdownApp site)

shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no Yesod app running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
