{-# LANGUAGE FlexibleContexts #-}
module Language.Java.Codec
       ( DecodeState (..)
       , EncodeState (..)
       , Decoder
       , Encoder
       , decode
       , encode
       ) where

import           Control.Monad.State.Strict (evalState)
import qualified Codec.Class as C (Decoder, Encoder, Decode (..), Encode (..), decode, encode)
import           Data.ByteString.Lazy (ByteString)

data DecodeState = DecodeState
data EncodeState = EncodeState

type Decoder a = C.Decoder DecodeState a
type Encoder = C.Encoder EncodeState

decode :: C.Decode DecodeState a => ByteString -> a
decode a = evalState (C.decode a) DecodeState

encode :: C.Encode EncodeState a => a -> ByteString
encode a = evalState (C.encode a) EncodeState