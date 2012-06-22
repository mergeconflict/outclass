{-# LANGUAGE TypeSynonymInstances #-}

module Language.Java.Codec
       ( Codec (..)
       , decode
       , encode
       ) where

import           Language.Java.Constants
import           Language.Java.Codec.Decoder
import           Language.Java.Codec.Encoder
import           Language.Java.Codec.Refs

import           Control.Monad.RWS
import qualified Data.Binary as Binary
import           Data.ByteString.Lazy
import           Data.IntMap
import           Data.Word

class Codec a where
  dec :: Decoder a
  enc :: a -> Encoder

decode :: Codec a => ByteString -> (a, Refs)
decode = runDecoder dec

encode :: Codec a => (a, Refs) -> ByteString
encode (a, refs) = runEncoder (enc a, refs)

type U1 = Word8
type U2 = Word16
type U4 = Word32
type U8 = Word64

instance Codec U1 where
  dec = lift Binary.get
  enc = lift . Binary.put

instance Codec U2 where
  dec = lift Binary.get
  enc = lift . Binary.put

instance Codec U4 where
  dec = lift Binary.get
  enc = lift . Binary.put

instance Codec U8 where
  dec = lift Binary.get
  enc = lift . Binary.put

decU2ref :: (Refs -> IntMap a) -> Decoder a
decU2ref f = do
  ref <- dec :: Decoder U2
  refs <- ask
  return $ (f refs) ! (fromIntegral ref)

decUtf8 :: Decoder Utf8C
decUtf8 = decU2ref (utf8s . constants)

instance Codec ClassC where
  dec = liftM Class decUtf8
  enc = undefined
