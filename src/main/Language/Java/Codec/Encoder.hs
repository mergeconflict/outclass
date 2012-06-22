module Language.Java.Codec.Encoder
       ( Encoder
       , runEncoder
       , byteStringEncoder
       ) where

import           Language.Java.Codec.Refs

import           Control.Monad.Reader
import           Data.Binary.Put
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

type Encoder = ReaderT Refs PutM ()

runEncoder :: (Encoder, Refs) -> Lazy.ByteString
runEncoder (r, refs) = runPut (runReaderT r refs)

byteStringEncoder :: Strict.ByteString -> Encoder
byteStringEncoder = lift . putByteString
