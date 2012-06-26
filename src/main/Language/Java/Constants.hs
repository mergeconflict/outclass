{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Constants
       ( ClassC (..)
       , FieldC (..)
       , MethodC (..)
       , InterfaceMethodC (..)
       , StringC (..)
       , IntegerC (..)
       , FloatC (..)
       , LongC (..)
       , DoubleC (..)
       , NameAndTypeC (..)
       , Utf8C (..)
       , MethodHandleC (..)
       , MethodTypeC (..)
       , InvokeDynamicC (..)
       , Constant (..)
       ) where

import Data.Hashable
import Data.Int

newtype ClassC           = Class            Utf8C                deriving (Eq, Hashable)
data    FieldC           = Field            ClassC  NameAndTypeC deriving Eq
data    MethodC          = Method           ClassC  NameAndTypeC deriving Eq
data    InterfaceMethodC = InterfaceMethod  ClassC  NameAndTypeC deriving Eq
newtype StringC          = String           Utf8C                deriving (Eq, Hashable)
newtype IntegerC         = Integer          Int32                deriving (Eq, Hashable)
newtype FloatC           = Float            Float                deriving (Eq, Hashable)
newtype LongC            = Long             Int64                deriving (Eq, Hashable)
newtype DoubleC          = Double           Double               deriving (Eq, Hashable)
data    NameAndTypeC     = NameAndType      Utf8C   Utf8C        deriving Eq
newtype Utf8C            = Utf8             String               deriving (Eq, Hashable)
data    MethodHandleC    = GetField         FieldC  |
                           GetStatic        FieldC  |
                           PutField         FieldC  |
                           PutStatic        FieldC  |
                           InvokeVirtual    MethodC |
                           InvokeStatic     MethodC |
                           InvokeSpecial    MethodC |
                           NewInvokeSpecial MethodC |
                           InvokeInterface  InterfaceMethodC     deriving Eq
newtype MethodTypeC      = MethodType       Utf8C                deriving (Eq, Hashable)
data InvokeDynamicC      = InvokeDynamic    ()      NameAndTypeC deriving Eq
  -- TODO () == BootstrapMethod

data Constant =
  ClassConstant           ClassC           |
  FieldConstant           FieldC           |
  MethodConstant          MethodC          |
  InterfaceMethodConstant InterfaceMethodC |
  StringConstant          StringC          |
  IntegerConstant         IntegerC         |
  FloatConstant           FloatC           |
  LongConstant            LongC            |
  DoubleConstant          DoubleC          |
  NameAndTypeConstant     NameAndTypeC     |
  Utf8Constant            Utf8C            |
  MethodHandleConstant    MethodHandleC    |
  MethodTypeConstant      MethodTypeC      |
  InvokeDynamicConstant   InvokeDynamicC
  deriving Eq

hash2 :: (Hashable a, Hashable b) => a -> b -> Int
hash2 a b = (hash a) `combine` (hash b)

instance Hashable FieldC where
  hash (Field c nt) = hash2 c nt

instance Hashable MethodC where
  hash (Method c nt) = hash2 c nt

instance Hashable InterfaceMethodC where
  hash (InterfaceMethod c nt) = hash2 c nt

instance Hashable NameAndTypeC where
  hash (NameAndType n t) = hash2 n t

instance Hashable MethodHandleC where
  hash (GetField f)         = hashWithSalt 1 f
  hash (GetStatic f)        = hashWithSalt 2 f
  hash (PutField f)         = hashWithSalt 3 f
  hash (PutStatic f)        = hashWithSalt 4 f
  hash (InvokeVirtual m)    = hashWithSalt 5 m
  hash (InvokeStatic m)     = hashWithSalt 6 m
  hash (InvokeSpecial m)    = hashWithSalt 7 m
  hash (NewInvokeSpecial m) = hashWithSalt 8 m
  hash (InvokeInterface im) = hashWithSalt 9 im

instance Hashable InvokeDynamicC where
  hash (InvokeDynamic bm nt) = hash2 bm nt

instance Hashable Constant where
  hash (ClassConstant c)           = hashWithSalt 7  c
  hash (FieldConstant c)           = hashWithSalt 9  c
  hash (MethodConstant c)          = hashWithSalt 10 c
  hash (InterfaceMethodConstant c) = hashWithSalt 11 c
  hash (StringConstant c)          = hashWithSalt 8  c
  hash (IntegerConstant c)         = hashWithSalt 3  c
  hash (FloatConstant c)           = hashWithSalt 4  c
  hash (LongConstant c)            = hashWithSalt 5  c
  hash (DoubleConstant c)          = hashWithSalt 6  c
  hash (NameAndTypeConstant c)     = hashWithSalt 12 c
  hash (Utf8Constant c)            = hashWithSalt 1  c
  hash (MethodHandleConstant c)    = hashWithSalt 15 c
  hash (MethodTypeConstant c)      = hashWithSalt 16 c
  hash (InvokeDynamicConstant c)   = hashWithSalt 18 c