module Language.Java.Codec.Decoder
       ( DecState (..)
       , Decoder
       , runDecoder
       , byteStringDecoder
       ) where

import           Language.Java.Codec.Refs

import           Control.Monad.RWS
import           Data.Binary.Get
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

data DecState = DecState
type Decoder a = RWST Refs Refs DecState Get a

runDecoder :: Decoder a -> Lazy.ByteString -> a
runDecoder m b =
  let knot ~(_, _, r) = runRWST m r DecState
      (a, _, _) = runGet (mfix knot) b
  in a

byteStringDecoder :: Int -> Decoder Strict.ByteString
byteStringDecoder = lift . getByteString
