module Language.Java.Codec.Decoder
       ( DecState (..)
       , Decoder
       , runDecoder
       , byteStringDecoder
       , DConstantRefs (..)
       , DRefs (..)
       ) where

import           Language.Java.Constants

import           Control.Monad.RWS
import           Data.Binary.Get
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
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

data DConstantRefs = DConstantRefs {
  classes          :: IntMap ClassC,
  fields           :: IntMap FieldC,
  methods          :: IntMap MethodC,
  interfaceMethods :: IntMap InterfaceMethodC,
  strings          :: IntMap StringC,
  integers         :: IntMap IntegerC,
  floats           :: IntMap FloatC,
  longs            :: IntMap LongC,
  doubles          :: IntMap DoubleC,
  nameAndTypes     :: IntMap NameAndTypeC,
  utf8s            :: IntMap Utf8C,
  methodHandles    :: IntMap MethodHandleC,
  methodTypes      :: IntMap MethodTypeC,
  invokeDynamics   :: IntMap InvokeDynamicC
}

instance Monoid DConstantRefs where
  mempty = DConstantRefs {
    classes          = mempty,
    fields           = mempty,
    methods          = mempty,
    interfaceMethods = mempty,
    strings          = mempty,
    integers         = mempty,
    floats           = mempty,
    longs            = mempty,
    doubles          = mempty,
    nameAndTypes     = mempty,
    utf8s            = mempty,
    methodHandles    = mempty,
    methodTypes      = mempty,
    invokeDynamics   = mempty
  }
  mappend a b = DConstantRefs {
    classes          = mappend (classes a)          (classes b),
    fields           = mappend (fields a)           (fields b),
    methods          = mappend (methods a)          (methods b),
    interfaceMethods = mappend (interfaceMethods a) (interfaceMethods b),
    strings          = mappend (strings a)          (strings b),
    integers         = mappend (integers a)         (integers b),
    floats           = mappend (floats a)           (floats b),
    longs            = mappend (longs a)            (longs b),
    doubles          = mappend (doubles a)          (doubles b),
    nameAndTypes     = mappend (nameAndTypes a)     (nameAndTypes b),
    utf8s            = mappend (utf8s a)            (utf8s b),
    methodHandles    = mappend (methodHandles a)    (methodHandles b),
    methodTypes      = mappend (methodTypes a)      (methodTypes b),
    invokeDynamics   = mappend (invokeDynamics a)   (invokeDynamics b)
  }

data DRefs = DRefs {
  constants :: DConstantRefs
}

instance Monoid DRefs where
  mempty = DRefs {
    constants = mempty
  }
  mappend a b = DRefs {
    constants = mappend (constants a) (constants b)
  }
