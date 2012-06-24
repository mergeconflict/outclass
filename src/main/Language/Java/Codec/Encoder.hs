module Language.Java.Codec.Encoder
       ( Encoder
       , runEncoder
       , byteStringEncoder
       ) where

import           Language.Java.Codec.Refs

import           Control.Monad.RWS
import           Control.Monad.Writer
import           Data.Binary.Builder
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

data EncState = EncState
type Encoder = RWST Refs Refs EncState (Writer Builder) ()

runEncoder :: Encoder -> Lazy.ByteString
runEncoder m =
  let knot ~(_, _, r) = runRWST m r EncState
      (_, w) = runWriter (mfix knot)
  in toLazyByteString w

byteStringEncoder :: Strict.ByteString -> Encoder
byteStringEncoder = lift . tell . fromByteString
