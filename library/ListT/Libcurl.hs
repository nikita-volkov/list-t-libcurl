module ListT.Libcurl
(
  Session,
  Error,
  runSession,
  consumeURL,
)
where

import BasePrelude hiding (cons, uncons)
import Foreign hiding (Pool)
import MTLPrelude hiding (Error)
import Control.Monad.Trans.Either hiding (left, right)
import ListT (ListT)
import Data.ByteString (ByteString)
import SlaveThread
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Pool as P
import qualified Network.CURL720 as C
import qualified ListT as L
import qualified Language.Haskell.TH.Syntax as TH


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
    producerMV <- newEmptyMVar
    C.curl_easy_setopt h
      [
        C.CURLOPT_WRITEFUNCTION $ Just (mVarWriteFunction producerMV),
        C.CURLOPT_URL url
      ]
    resultMV <- newEmptyMVar
    fork $ do
      putMVar resultMV =<< consumer (L.fromMVar producerMV)
    C.curl_easy_perform h
    putMVar producerMV Nothing
    takeMVar resultMV


mVarWriteFunction :: MVar (Maybe ByteString) -> C.CURL_write_callback
mVarWriteFunction v b = do
  putMVar v $ Just b
  return C.CURL_WRITEFUNC_OK


