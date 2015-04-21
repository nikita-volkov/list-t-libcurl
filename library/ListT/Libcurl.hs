module ListT.Libcurl where

import BasePrelude hiding (cons, uncons)
import Foreign hiding (Pool)
import MTLPrelude hiding (Error)
import Control.Monad.Trans.Either hiding (left, right)
import ListT (ListT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Pool as DP
import qualified Network.Curl as Curl
import qualified ListT as L
import qualified Language.Haskell.TH.Syntax as TH


newtype Pool s =
  Pool (DP.Pool Curl.Curl)

usePool :: PoolSettings -> (forall s. ReaderT (Pool s) IO a) -> IO a
usePool (PoolSettings size timeout) m =
  Curl.withCurlDo $ bracket acquire release $ runReaderT m . Pool
  where
    acquire =
      DP.createPool Curl.initialize (const (return ())) 1 (fromIntegral timeout) size
    release pool =
      DP.destroyAllResources pool


-- |
-- Settings of a pool.
data PoolSettings =
  PoolSettings !Int !Int
  deriving (Show)

instance TH.Lift PoolSettings where
  lift (PoolSettings a b) = 
    [|PoolSettings a b|]

-- | 
-- A smart constructor for pool settings.
poolSettings :: 
  Int
  -- ^
  -- The maximum number of connections to keep open. 
  -- The smallest acceptable value is 1.
  -- Requests for connections will block if this limit is reached.
  -> 
  Int
  -- ^
  -- The amount of seconds for which an unused connection is kept open. 
  -- The smallest acceptable value is 1.
  -> 
  Maybe PoolSettings
  -- ^
  -- Maybe pool settings, if they are correct.
poolSettings size timeout =
  if size > 0 && timeout >= 1
    then Just $ PoolSettings size timeout
    else Nothing


-- |
-- Presented as a specialised monad to prohibit the concurrent use of a single session handle.
newtype Session a =
  Session (ReaderT Curl.Curl IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runSession :: Session a -> ReaderT (Pool s) IO a
runSession (Session m) =
  ReaderT $ \(Pool pool) -> DP.withResource pool $ runReaderT m


urlExecutor :: String -> (ListT IO ByteString -> IO a) -> Session (Either Curl.CurlCode a)
urlExecutor url handler =
  Session $ ReaderT $ \h -> do
    Curl.setopt h $ Curl.CurlFailOnError True
    Curl.setDefaultSSLOpts h $ url
    Curl.setopt h $ Curl.CurlURL url

    producerMV <- newEmptyMVar
    Curl.setopt h $ Curl.CurlWriteFunction $ chunkHandlerWriteFunction $ 
      \b -> putMVar producerMV $ bool Nothing (Just b) $ B.null b

    Curl.perform h >>= \case
      Curl.CurlOK -> do
        result <- handler $ L.fromMVar producerMV
        return $ Right result
      code -> do
        return $ Left code

writeFunction :: ((Ptr Word8, Int) -> IO ()) -> Curl.WriteFunction
writeFunction f src sz nelems _ = do
    let n' = sz * nelems
    f (castPtr src, fromIntegral n')
    return n'

chunkHandlerWriteFunction :: (ByteString -> IO ()) -> Curl.WriteFunction
chunkHandlerWriteFunction handler =
  writeFunction $ \(ptr, size) -> do
    bs <- BU.unsafePackCStringFinalizer ptr size (free ptr)
    handler bs

