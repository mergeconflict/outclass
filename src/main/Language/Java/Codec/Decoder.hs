{-# LANGUAGE Rank2Types #-}

module Language.Java.Codec.Decoder
       ( State (..)
       , Decoder
       , runDecoder
       , binaryDecoder
       , byteStringDecoder
       , Refs (..)
       ) where

import           Control.Monad.RWS
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.Binary.Get
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable
import           Data.HashMap.Lazy
import           Data.IntMap

data State = State -- TODO ConstantState U2, etc.
type Decoder a = RWST Refs Refs State Get a

runDecoder :: Decoder a -> State -> Lazy.ByteString -> a
runDecoder m s b =
  let knot ~(_, _, r) = runRWST m r s
      (a, _, _) = runGet (mfix knot) b
  in a

binaryDecoder :: Binary a => Decoder a
binaryDecoder = lift Binary.get

byteStringDecoder :: Int -> Decoder Strict.ByteString
byteStringDecoder = lift . getByteString

data Refs = Refs {
  constants :: Typeable a => HashMap TypeRep (IntMap a)
}

instance Monoid Refs where
  mempty = Refs {
    constants = mempty
  }
  mappend a b = Refs {
    constants = mappend (constants a) (constants b)
  }
