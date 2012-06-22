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

import Data.Int

data ClassC           = Class Utf8C
data FieldC           = Field ClassC NameAndTypeC
data MethodC          = Method ClassC NameAndTypeC
data InterfaceMethodC = InterfaceMethod ClassC NameAndTypeC
data StringC          = String Utf8C
data IntegerC         = Integer Int32
data FloatC           = Float Float
data LongC            = Long Int64
data DoubleC          = Double Double
data NameAndTypeC     = NameAndType Utf8C Utf8C
data Utf8C            = Utf8 String
data MethodHandleC    =
  GetField         FieldC  |
  GetStatic        FieldC  |
  PutField         FieldC  |
  PutStatic        FieldC  |
  InvokeVirtual    MethodC |
  InvokeStatic     MethodC |
  InvokeSpecial    MethodC |
  NewInvokeSpecial MethodC |
  InvokeInterface  InterfaceMethodC
data MethodTypeC      = MethodType Utf8C
data InvokeDynamicC   = InvokeDynamic () NameAndTypeC -- TODO () == BootstrapMethod

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