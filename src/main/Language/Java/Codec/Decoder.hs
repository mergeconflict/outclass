{-# LANGUAGE Rank2Types #-}

module Language.Java.Codec.Decoder
       ( DecState (..)
       , Decoder
       , runDecoder
       , byteStringDecoder
       , DRefs (..)
       ) where

import           Control.Monad.RWS
import           Data.Binary.Get
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable
import           Data.HashMap.Lazy
import           Data.IntMap

data DecState = DecState
type Decoder a = RWST DRefs DRefs DecState Get a

runDecoder :: Decoder a -> Lazy.ByteString -> a
runDecoder m b =
  let knot ~(_, _, r) = runRWST m r DecState
      (a, _, _) = runGet (mfix knot) b
  in a

byteStringDecoder :: Int -> Decoder Strict.ByteString
byteStringDecoder = lift . getByteString

data DRefs = DRefs {
  constants :: Typeable a => HashMap TypeRep (IntMap a)
}

instance Monoid DRefs where
  mempty = DRefs {
    constants = mempty
  }
  mappend a b = DRefs {
    constants = mappend (constants a) (constants b)
  }
