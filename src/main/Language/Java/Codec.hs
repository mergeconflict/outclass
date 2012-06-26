{-# LANGUAGE TypeSynonymInstances #-}

module Language.Java.Codec
       ( Codec (..)
       , decode
       , encode
       ) where

import           Language.Java.Constants
import           Language.Java.Codec.Bytes
import           Language.Java.Codec.Decoder (Decoder, DRefs, runDecoder)
import qualified Language.Java.Codec.Decoder as D
import           Language.Java.Codec.Encoder (Encoder, ERefs, runEncoder)
import qualified Language.Java.Codec.Encoder as E

import           Control.Applicative
import           Control.Monad.RWS
import qualified Data.Binary as Binary
import           Data.Binary.Builder as Builder
import           Data.ByteString.Lazy
import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

class Codec a where
  dec :: Decoder a
  enc :: a -> Encoder

decode :: Codec a => ByteString -> a
decode = runDecoder dec

encode :: Codec a => a -> ByteString
encode = runEncoder . enc

{- Bytes -}

instance Codec U1 where
  dec = lift Binary.get
  enc = lift . tell . Builder.singleton

instance Codec U2 where
  dec = lift Binary.get
  enc = lift . tell . Builder.putWord16be

instance Codec U4 where
  dec = lift Binary.get
  enc = lift . tell . Builder.putWord32be

instance Codec U8 where
  dec = lift Binary.get
  enc = lift . tell . Builder.putWord64be

{- References -}

decU2ref :: (DRefs -> IntMap a) -> Decoder a
decU2ref f = do
  ref <- fromIntegral <$> (dec :: Decoder U2)
  m   <- f <$> ask
  pure $ m IntMap.! ref

encU2ref :: (Eq a, Hashable a) => (ERefs -> HashMap a U2) -> a -> Encoder
encU2ref f a = do
  m <- f <$> ask
  enc $ m HashMap.! a

{- Constants -}

decUtf8ref :: Decoder Utf8C
decUtf8ref = decU2ref (D.utf8s . D.constants)

encUtf8ref :: Utf8C -> Encoder
encUtf8ref a = encU2ref (E.utf8s . E.constants) a

instance Codec ClassC where
  dec = Class <$> decUtf8ref
  enc (Class n) = encUtf8ref n
