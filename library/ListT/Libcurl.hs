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
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Pool as P
import qualified Network.Curl as C
import qualified ListT as L
import qualified Language.Haskell.TH.Syntax as TH


-- |
-- A global sessions pool.
-- 
-- Due to how the \"libcurl\" library integration is handled,
-- there may only exist one per application, 
-- hence the API provides no way to establish another pool.
{-# NOINLINE pool #-}
pool :: P.Pool C.Curl
pool =
  unsafePerformIO $ P.createPool acquire release 1 30 100
  where
    acquire = do
      h <- C.initialize
      C.setopt h $ C.CurlFailOnError True
      return h
    release = const $ return ()


-- |
-- A monad for sequential execution of \"libcurl\" operations.
-- 
-- Intentionally prohibits the concurrent use due to the way the integration
-- with \"libcurl\" is handled.
newtype Session a =
  Session (ReaderT C.Curl (EitherT Error IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

type Error =
  C.CurlCode


runSession :: Session a -> IO (Either Error a)
runSession (Session m) =
  P.withResource pool $ runEitherT . runReaderT m

consumeURL :: String -> (ListT IO ByteString -> IO a) -> Session a
consumeURL url consumer =
  Session $ ReaderT $ \h -> EitherT $ do
    C.setDefaultSSLOpts h $ url
    C.setopt h $ C.CurlURL url

    producerMV <- newEmptyMVar
    C.setopt h $ C.CurlWriteFunction $ chunkHandlerWriteFunction $ 
      \b -> putMVar producerMV $ bool Nothing (Just b) $ B.null b

    C.perform h >>= \case
      C.CurlOK -> do
        result <- consumer $ L.fromMVar producerMV
        return $ Right result
      code -> do
        return $ Left code

writeFunction :: ((Ptr Word8, Int) -> IO ()) -> C.WriteFunction
writeFunction f src sz nelems _ = do
  let n' = sz * nelems
  f (castPtr src, fromIntegral n')
  return n'

chunkHandlerWriteFunction :: (ByteString -> IO ()) -> C.WriteFunction
chunkHandlerWriteFunction handler =
  writeFunction $ \(ptr, size) -> do
    bs <- BU.unsafePackCStringFinalizer ptr size (free ptr)
    handler bs

