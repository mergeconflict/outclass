module Language.Java.Codec.Refs
       ( ConstantRefs (..)
       , Refs (..)
       ) where

import Language.Java.Constants

import Data.IntMap
import Data.Monoid

data ConstantRefs = ConstantRefs {
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

instance Monoid ConstantRefs where
  mempty = ConstantRefs {
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
  mappend a b = ConstantRefs {
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

data Refs = Refs {
  constants :: ConstantRefs
}

instance Monoid Refs where
  mempty = Refs {
    constants = mempty
  }
  mappend a b = Refs {
    constants = mappend (constants a) (constants b)
  }
