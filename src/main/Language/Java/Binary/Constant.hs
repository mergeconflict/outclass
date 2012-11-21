{-# LANGUAGE NamedFieldPuns #-}

module Language.Java.Binary.Constant
       ( MethodHandleKind (..)
       , Constant (..)
       ) where

import Language.Java.Binary.Primitive
import Language.Java.Binary.Word

import Control.Applicative
import Data.Binary

{- method handle kinds -}

data MethodHandleKind =
  GetField         |
  GetStatic        |
  PutField         |
  PutStatic        |
  InvokeVirtual    |
  InvokeStatic     |
  InvokeSpecial    |
  NewInvokeSpecial |
  InvokeInterface
  deriving (Eq, Show)

instance Binary MethodHandleKind where
  get = do
    kind <- get :: Get U1
    case kind of
      1 -> return GetField
      2 -> return GetStatic
      3 -> return PutField
      4 -> return PutStatic
      5 -> return InvokeVirtual
      6 -> return InvokeStatic
      7 -> return InvokeSpecial
      8 -> return NewInvokeSpecial
      9 -> return InvokeInterface
      _ -> fail "invalid MethodHandleKind"
  put kind =
    (put :: U1 -> Put) $ case kind of
      GetField         -> 1
      GetStatic        -> 2
      PutField         -> 3
      PutStatic        -> 4
      InvokeVirtual    -> 5
      InvokeStatic     -> 6
      InvokeSpecial    -> 7
      NewInvokeSpecial -> 8
      InvokeInterface  -> 9

{- constants -}

data Constant =
  ClassConstant
    { nameIndex :: U2 } |
  FieldConstant
    { classIndex :: U2, nameAndTypeIndex :: U2 } |
  MethodConstant
    { classIndex :: U2, nameAndTypeIndex :: U2 } |
  InterfaceMethodConstant
    { classIndex :: U2, nameAndTypeIndex :: U2 } |
  StringConstant
    { utf8Index :: U2 } |
  IntegerConstant
    { integerValue :: JInt } |
  FloatConstant
    { floatValue :: JFloat } |
  LongConstant
    { longValue :: JLong } |
  DoubleConstant
    { doubleValue :: JDouble } |
  NameAndTypeConstant
    { nameIndex :: U2, descriptorIndex :: U2 } |
  Utf8Constant
    { utf8Value :: JString } |
  MethodHandleConstant
    { referenceKind :: MethodHandleKind, referenceIndex :: U2} |
  MethodTypeConstant
    { descriptorIndex :: U2 } |
  InvokeDynamicConstant
    { bootstrapMethodAttrIndex :: U2, nameAndTypeIndex :: U2 }
  deriving (Eq, Show)

instance Binary Constant where
  get = do
    tag <- get :: Get U1
    case tag of
      7  -> ClassConstant <$> get
      9  -> FieldConstant <$> get <*> get
      10 -> MethodConstant <$> get <*> get
      11 -> InterfaceMethodConstant <$> get <*> get
      8  -> StringConstant <$> get
      3  -> IntegerConstant <$> get
      4  -> FloatConstant <$> get
      5  -> LongConstant <$> get
      6  -> DoubleConstant <$> get
      12 -> NameAndTypeConstant <$> get <*> get
      1  -> Utf8Constant <$> get
      15 -> MethodHandleConstant <$> get <*> get
      16 -> MethodTypeConstant <$> get
      18 -> InvokeDynamicConstant <$> get <*> get
      _  -> fail "invalid Constant"

  put (ClassConstant nameIndex) =
    put (7 :: U1) >> put nameIndex
  put (FieldConstant classIndex nameAndTypeIndex) =
    put (9 :: U1) >> put classIndex >> put nameAndTypeIndex
  put (MethodConstant classIndex nameAndTypeIndex) =
    put (10 :: U1) >> put classIndex >> put nameAndTypeIndex
  put (InterfaceMethodConstant classIndex nameAndTypeIndex) =
    put (11 :: U1) >> put classIndex >> put nameAndTypeIndex
  put (StringConstant utf8Index) =
    put (8 :: U1) >> put utf8Index
  put (IntegerConstant integerValue) =
    put (3 :: U1) >> put integerValue
  put (FloatConstant floatValue) =
    put (4 :: U1) >> put floatValue
  put (LongConstant longValue) =
    put (5 :: U1) >> put longValue
  put (DoubleConstant doubleValue) =
    put (6 :: U1) >> put doubleValue
  put (NameAndTypeConstant nameIndex descriptorIndex) =
    put (12 :: U1) >> put nameIndex >> put descriptorIndex
  put (Utf8Constant utf8Value) =
    put (1 :: U1) >> put utf8Value
  put (MethodHandleConstant referenceKind referenceIndex) =
    put (15 :: U1) >> put referenceKind >> put referenceIndex
  put (MethodTypeConstant descriptorIndex) =
    put (16 :: U1) >> put descriptorIndex
  put (InvokeDynamicConstant bootstrapMethodAttrIndex nameAndTypeIndex) =
    put (18 :: U1) >> put bootstrapMethodAttrIndex >> put nameAndTypeIndex
