{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module Codec.Class
       ( Decode (..)
       , Encode (..)
       , Decoder
       , Encoder
       , decode
       , encode
       ) where

import Codec.Decoder
import Codec.Encoder

import Control.Monad.State.Strict (State, lift)
import Data.Binary (Binary, get, put)
import Data.ByteString.Lazy (ByteString)

class Decode s a where
  decoder :: Decoder s a

instance Binary a => Decode s a where
  decoder = lift get

class Encode s a where
  encoder :: a -> Encoder s

instance Binary a => Encode s a where
  encoder = lift . put

decode :: Decode s a => ByteString -> State s a
decode = runDecoder decoder

encode :: Encode s a => a -> State s ByteString
encode = runEncoder . encoder
