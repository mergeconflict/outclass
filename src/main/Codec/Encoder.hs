module Codec.Encoder
       ( Encoder
       , runEncoder
       , byteStringEncoder
       ) where

import           Control.Monad.State.Strict (State, StateT, get, lift, put, runStateT)
import           Data.Binary.Put (PutM, putByteString, runPutM)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

type Encoder s = StateT s PutM ()

runEncoder :: Encoder s -> State s Lazy.ByteString
runEncoder e = do
  s <- get
  let p = runStateT e s
      ((_, s'), b) = runPutM p
  put s'
  return b

byteStringEncoder :: Strict.ByteString -> Encoder s
byteStringEncoder = lift . putByteString
