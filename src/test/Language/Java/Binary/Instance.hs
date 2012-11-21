{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Language.Java.Binary.Instance () where

import Language.Java.Binary.Constant
import Language.Java.Binary.Primitive
import Language.Java.Binary.Word

import Control.Applicative
import Test.QuickCheck

instance Arbitrary MethodHandleKind where
  arbitrary = elements
    [ GetField
    , GetStatic
    , PutField
    , PutStatic
    , InvokeVirtual
    , InvokeStatic
    , InvokeSpecial
    , NewInvokeSpecial
    , InvokeInterface
    ]

instance Arbitrary Constant where
  arbitrary = oneof
    [ ClassConstant <$> arbitrary 
    , FieldConstant <$> arbitrary <*> arbitrary
    , MethodConstant <$> arbitrary <*> arbitrary
    , InterfaceMethodConstant <$> arbitrary <*> arbitrary
    , StringConstant <$> arbitrary
    , IntegerConstant <$> arbitrary
    , FloatConstant <$> arbitrary
    , LongConstant <$> arbitrary
    , DoubleConstant <$> arbitrary
    , NameAndTypeConstant <$> arbitrary <*> arbitrary
    , Utf8Constant <$> arbitrary
    , MethodHandleConstant <$> arbitrary <*> arbitrary
    , MethodTypeConstant <$> arbitrary
    , InvokeDynamicConstant <$> arbitrary <*> arbitrary
    ]

deriving instance Arbitrary JDouble
deriving instance Arbitrary JFloat
deriving instance Arbitrary JInt
deriving instance Arbitrary JLong
deriving instance Arbitrary JString

deriving instance Arbitrary U1
deriving instance Arbitrary U2
deriving instance Arbitrary U4
deriving instance Arbitrary U8
