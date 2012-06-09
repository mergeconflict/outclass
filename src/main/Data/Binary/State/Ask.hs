module Data.Binary.State.Ask
       ( Ask
       , runAsk
       , askWord8
       , askByteString
       , askLazyByteString
       ) where

import           Control.Monad.State.Strict (State, StateT, get, lift, put, runStateT)
import qualified Data.Binary.Get as B
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Int
import           Data.Word

type Ask s a = StateT s B.Get a

runAsk :: Ask s a -> Lazy.ByteString -> State s a
runAsk a b = do
  s <- get
  let g = runStateT a s
      (v, s') = B.runGet g b
  put s'
  return v

askWord8 :: Ask s Word8
askWord8 = lift B.getWord8

askByteString :: Int -> Ask s Strict.ByteString
askByteString = lift . B.getByteString

askLazyByteString :: Int64 -> Ask s Lazy.ByteString
askLazyByteString = lift . B.getLazyByteString