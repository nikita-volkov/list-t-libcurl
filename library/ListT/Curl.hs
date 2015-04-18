module ListT.Curl where

import BasePrelude hiding (cons, uncons)
import Foreign
import MTLPrelude hiding (Error)
import Control.Monad.Trans.Either hiding (left, right)
import ListT (ListT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Network.Curl as Curl
import qualified ListT as L


type Error =
  Curl.CurlCode

urlExecutor :: String -> (ListT IO ByteString -> IO a) -> IO (Either Error a)
urlExecutor url handler =
  Curl.withCurlDo $ do
    h <- Curl.initialize

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

