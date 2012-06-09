module Data.Binary.State.Tell
       ( TellM
       , Tell
       , runTell
       , tellWord8
       , tellByteString
       , tellLazyByteString
       ) where

import           Control.Monad.State.Strict (State, StateT, get, lift, put, runStateT)
import qualified Data.Binary.Put as B
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Word

type TellM s a = StateT s B.PutM a
type Tell s = TellM s ()

runTell :: Tell s -> State s Lazy.ByteString
runTell t = do
  s <- get
  let p = runStateT t s
      ((_, s'), b) = B.runPutM p
  put s'
  return b

tellWord8 :: Word8 -> Tell s
tellWord8 = lift . B.putWord8

tellByteString :: Strict.ByteString -> Tell s
tellByteString = lift . B.putByteString

tellLazyByteString :: Lazy.ByteString -> Tell s
tellLazyByteString = lift . B.putLazyByteString