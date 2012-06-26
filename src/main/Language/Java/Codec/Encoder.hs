{-# LANGUAGE Rank2Types #-}

module Language.Java.Codec.Encoder
       ( Encoder
       , runEncoder
       , byteStringEncoder
       , ERefs (..)
       ) where

import           Language.Java.Codec.Bytes

import           Control.Monad.RWS
import           Control.Monad.Writer
import           Data.Binary.Builder
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.HashMap.Lazy
import           Data.Typeable

data EncState = EncState
type Encoder = RWST ERefs ERefs EncState (Writer Builder) ()

runEncoder :: Encoder -> Lazy.ByteString
runEncoder m =
  let knot ~(_, _, r) = runRWST m r EncState
      (_, w) = runWriter (mfix knot)
  in toLazyByteString w

byteStringEncoder :: Strict.ByteString -> Encoder
byteStringEncoder = lift . tell . fromByteString

data ERefs = ERefs {
  constants :: Typeable a => HashMap TypeRep (HashMap a U2)
}

instance Monoid ERefs where
  mempty = ERefs {
    constants = mempty
  }
  mappend a b = ERefs {
    constants = mappend (constants a) (constants b)
  }
