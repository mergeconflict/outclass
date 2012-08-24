{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Codec.Constants
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

import Language.Java.Codec.Bytes
import Language.Java.Codec.Constants.Utf8

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int

{- constant types -}

newtype ClassC           = Class            U2     deriving (Binary, Eq)
data    FieldC           = Field            U2 U2  deriving Eq
data    MethodC          = Method           U2 U2  deriving Eq
data    InterfaceMethodC = InterfaceMethod  U2 U2  deriving Eq
newtype StringC          = String           U2     deriving (Binary, Eq)
newtype IntegerC         = Integer          Int32  deriving (Binary, Eq)
newtype FloatC           = Float            Float  deriving (Binary, Eq)
newtype LongC            = Long             Int64  deriving (Binary, Eq)
newtype DoubleC          = Double           Double deriving (Binary, Eq)
data    NameAndTypeC     = NameAndType      U2 U2  deriving Eq
newtype Utf8C            = Utf8             String deriving Eq
data    MethodHandleC    = GetField         U2     |
                           GetStatic        U2     |
                           PutField         U2     |
                           PutStatic        U2     |
                           InvokeVirtual    U2     |
                           InvokeStatic     U2     |
                           InvokeSpecial    U2     |
                           NewInvokeSpecial U2     |
                           InvokeInterface  U2     deriving Eq
newtype MethodTypeC      = MethodType       U2     deriving (Binary, Eq)
data    InvokeDynamicC   = InvokeDynamic    U2 U2  deriving Eq

instance Binary FieldC where
  get = get2 Field
  put (Field c nt) = put2 c nt

instance Binary MethodC where
  get = get2 Method
  put (Method c nt) = put2 c nt

instance Binary InterfaceMethodC where
  get = get2 InterfaceMethod
  put (InterfaceMethod c nt) = put2 c nt

instance Binary Utf8C where
  get = do
    len <- get :: Get U2
    str <- getByteString $ fromIntegral len
    case decodeString str of
      Just u  -> return $ Utf8 u
      Nothing -> fail "invalid utf-8 constant"
  put (Utf8 str) = putByteString $ encodeString str

instance Binary NameAndTypeC where
  get = get2 NameAndType
  put (NameAndType c nt) = put2 c nt

instance Binary MethodHandleC where
  get = do
    kind <- get :: Get U1
    let
      go :: (U2 -> MethodHandleC) -> Get MethodHandleC
      go c = c <$> get
    case kind of
      1 -> go GetField
      2 -> go GetStatic
      3 -> go PutField
      4 -> go PutStatic
      5 -> go InvokeVirtual
      6 -> go InvokeStatic
      7 -> go InvokeSpecial
      8 -> go NewInvokeSpecial
      9 -> go InvokeInterface
      _ -> fail "invalid method handle kind"

  put mh =
    let
      go :: U1 -> U2 -> Put
      go kind ref = put kind >> put ref
    in case mh of
      GetField ref         -> go 1 ref
      GetStatic ref        -> go 2 ref
      PutField ref         -> go 3 ref
      PutStatic ref        -> go 4 ref
      InvokeVirtual ref    -> go 5 ref
      InvokeStatic ref     -> go 6 ref
      InvokeSpecial ref    -> go 7 ref
      NewInvokeSpecial ref -> go 8 ref
      InvokeInterface ref  -> go 9 ref

instance Binary InvokeDynamicC where
  get = get2 InvokeDynamic
  put (InvokeDynamic c nt) = put2 c nt

{- constant -}

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

instance Binary Constant where
  get = do
    tag <- get :: Get U1
    let
      go :: Binary a => (a -> Constant) -> Get Constant
      go c = c <$> get
    case tag of
      7  -> go ClassConstant
      9  -> go FieldConstant
      10 -> go MethodConstant
      11 -> go InterfaceMethodConstant
      8  -> go StringConstant
      3  -> go IntegerConstant
      4  -> go FloatConstant
      5  -> go LongConstant
      6  -> go DoubleConstant
      12 -> go NameAndTypeConstant
      1  -> go Utf8Constant
      15 -> go MethodHandleConstant
      16 -> go MethodTypeConstant
      18 -> go InvokeDynamicConstant
      _  -> fail "invalid constant tag"

  put c =
    let
      go :: Binary a => U1 -> a -> Put
      go tag info = put tag >> put info
    in case c of
      ClassConstant info           -> go 7  info
      FieldConstant info           -> go 9  info
      MethodConstant info          -> go 10 info
      InterfaceMethodConstant info -> go 11 info
      StringConstant info          -> go 8  info
      IntegerConstant info         -> go 3  info
      FloatConstant info           -> go 4  info
      LongConstant info            -> go 5  info
      DoubleConstant info          -> go 6  info
      NameAndTypeConstant info     -> go 12 info
      Utf8Constant info            -> go 1  info
      MethodHandleConstant info    -> go 15 info
      MethodTypeConstant info      -> go 16 info
      InvokeDynamicConstant info   -> go 18 info

{- helpers -}

get2 :: (U2 -> U2 -> a) -> Get a
get2 c = c <$> get <*> get

put2 :: U2 -> U2 -> Put
put2 a b = put a >> put b
