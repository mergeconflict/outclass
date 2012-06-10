module Codec.Decoder
       ( Decoder
       , runDecoder
       , byteStringDecoder
       ) where

import           Control.Monad.State.Strict (State, StateT, get, lift, put, runStateT)
import           Data.Binary.Get (Get, getByteString, runGet)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

type Decoder s a = StateT s Get a

runDecoder :: Decoder s a -> Lazy.ByteString -> State s a
runDecoder d b = do
  s <- get
  let g = runStateT d s
      (v, s') = runGet g b
  put s'
  return v

byteStringDecoder :: Int -> Decoder s Strict.ByteString
byteStringDecoder = lift . getByteString
