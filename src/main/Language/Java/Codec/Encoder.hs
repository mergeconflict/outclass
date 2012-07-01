{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}

module Language.Java.Codec.Encoder
       ( State (..)
       , Encoder
       , runEncoder
       , binaryEncoder
       , byteStringEncoder
       , Refs (..)
       ) where

import           Language.Java.Codec.Bytes

import           Control.Applicative
import           Control.Monad.RWS
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.Binary.Put
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.HashMap.Lazy
import           Data.Typeable

newtype FixPutM a = Fix { unFix :: PutM a }
  deriving (Applicative, Functor, Monad)

runFixPutM :: FixPutM a -> (a, Lazy.ByteString)
runFixPutM = runPutM . unFix

instance MonadFix FixPutM where
  mfix f =
    let (a, b) = runFixPutM . f $ a
    in Fix $ putLazyByteString b >> return a

data State =
  ConstantState U2
type Encoder = RWST Refs Refs State FixPutM ()

runEncoder :: Encoder -> State -> Lazy.ByteString
runEncoder m s =
  let knot ~(_, _, r) = runRWST m r s
      (_, b) = runFixPutM $ mfix knot
  in b

binaryEncoder :: Binary a => a -> Encoder
binaryEncoder = lift . Fix . Binary.put

byteStringEncoder :: Strict.ByteString -> Encoder
byteStringEncoder = lift . Fix . putByteString

data Refs = Refs {
  constants :: Typeable a => HashMap TypeRep (HashMap a U2)
}

instance Monoid Refs where
  mempty = Refs {
    constants = mempty
  }
  mappend a b = Refs {
    constants = mappend (constants a) (constants b)
  }
