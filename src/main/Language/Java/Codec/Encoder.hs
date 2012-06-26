module Language.Java.Codec.Encoder
       ( Encoder
       , runEncoder
       , byteStringEncoder
       , EConstantRefs (..)
       , ERefs (..)
       ) where

import           Language.Java.Codec.Bytes
import           Language.Java.Constants

import           Control.Monad.RWS
import           Control.Monad.Writer
import           Data.Binary.Builder
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.HashMap.Lazy

data EncState = EncState
type Encoder = RWST ERefs ERefs EncState (Writer Builder) ()

runEncoder :: Encoder -> Lazy.ByteString
runEncoder m =
  let knot ~(_, _, r) = runRWST m r EncState
      (_, w) = runWriter (mfix knot)
  in toLazyByteString w

byteStringEncoder :: Strict.ByteString -> Encoder
byteStringEncoder = lift . tell . fromByteString

data EConstantRefs = EConstantRefs {
  classes          :: HashMap ClassC U2,
  fields           :: HashMap FieldC U2,
  methods          :: HashMap MethodC U2,
  interfaceMethods :: HashMap InterfaceMethodC U2,
  strings          :: HashMap StringC U2,
  integers         :: HashMap IntegerC U2,
  floats           :: HashMap FloatC U2,
  longs            :: HashMap LongC U2,
  doubles          :: HashMap DoubleC U2,
  nameAndTypes     :: HashMap NameAndTypeC U2,
  utf8s            :: HashMap Utf8C U2,
  methodHandles    :: HashMap MethodHandleC U2,
  methodTypes      :: HashMap MethodTypeC U2,
  invokeDynamics   :: HashMap InvokeDynamicC U2
}

instance Monoid EConstantRefs where
  mempty = EConstantRefs {
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
  mappend a b = EConstantRefs {
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

data ERefs = ERefs {
  constants :: EConstantRefs
}

instance Monoid ERefs where
  mempty = ERefs {
    constants = mempty
  }
  mappend a b = ERefs {
    constants = mappend (constants a) (constants b)
  }
