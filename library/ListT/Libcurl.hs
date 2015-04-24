module ListT.Libcurl
(
  Session,
  Error,
  runSession,
  consumeURL,
)
where

import BasePrelude hiding (cons, uncons)
import Foreign hiding (Pool, void)
import MTLPrelude hiding (Error)
import Control.Monad.Trans.Either hiding (left, right)
import ListT (ListT)
import Data.ByteString (ByteString)
import Control.Concurrent.STM.TMVar
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Pool as P
import qualified Network.CURL720 as C
import qualified ListT as L


-- |
-- A global sessions pool.
-- 
-- Due to how the \"libcurl\" library integration is handled,
-- there may only exist one per application, 
-- hence the API provides no way to establish another pool.
{-# NOINLINE pool #-}
pool :: P.Pool C.CURL
pool =
  unsafePerformIO $ P.createPool acquire release 1 30 100
  where
    acquire = do
      h <- C.curl_easy_init
      C.curl_easy_setopt h [C.CURLOPT_FAILONERROR True]
      return h
    release h = do
      C.curl_easy_cleanup h


-- |
-- A monad for sequential execution of \"libcurl\" operations.
-- 
-- To execute multiple requests concurrently you need to run multiple sessions.
newtype Session a =
  Session (ReaderT C.CURL IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

type Error =
  C.CURLE


runSession :: Session a -> IO (Either Error a)
runSession (Session m) =
  try $
  C.withlib C.CURL720 $
  P.withResource pool $ 
  runReaderT m

consumeURL :: String -> (ListT IO ByteString -> IO a) -> Session a
consumeURL url consumer =
  Session $ ReaderT $ \h -> do
    syncState@(active, chunk) <- atomically $ newSyncState
    C.curl_easy_setopt h
      [
        C.CURLOPT_WRITEFUNCTION $ Just (syncWriteFunction syncState),
        C.CURLOPT_URL url
      ]
    result <- newEmptyMVar :: IO (MVar (Either SomeException a))
    forkIO $ do
      r <- 
        try $ consumer $ fix $ \loop -> join $ lift $ atomically $
          tryTakeTMVar chunk >>= \case
            Just chunk -> return $ L.cons chunk loop
            _ -> readTVar active >>= \case
              False -> return mzero
              _ -> retry
      atomically $ writeTVar active False
      putMVar result r
    catch (C.curl_easy_perform h) $ \case
      C.CURLE _ _ _ C.CURLE_WRITE_ERROR -> return ()
      e -> throwIO e
    atomically $ writeTVar active False
    either (throwIO :: SomeException -> IO a) return =<< takeMVar result


type SyncState =
  (TVar Bool, TMVar ByteString)

newSyncState :: STM SyncState
newSyncState =
  (,) <$> newTVar True <*> newEmptyTMVar

syncWriteFunction :: SyncState -> C.CURL_write_callback
syncWriteFunction (active, chunk) b = 
  atomically $ do
    readTVar active >>= \case
      False -> return C.CURL_WRITEFUNC_FAIL
      True -> putTMVar chunk b >> return C.CURL_WRITEFUNC_OK


