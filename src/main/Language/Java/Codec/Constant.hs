{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Language.Java.Codec.Constant 
       ( Constant(..)
       ) where

import Language.Java.Bytes
import Language.Java.Codec
import Language.Java.Codec.Utf8

import           Codec.Class (Decode (..), Encode (..))
import           Codec.Decoder (byteStringDecoder)
import           Codec.Encoder (byteStringEncoder)
import           Control.Monad (liftM, liftM2)
import qualified Data.ByteString as B (length)

data Constant =
  Class U2 |
  Field U2 U2 |
  Method U2 U2 |
  InterfaceMethod U2 U2 |
  String U2 |
  Integer U4 |
  Float U4 |
  Long U8 |
  Double U8 |
  NameAndType U2 U2 |
  Utf8 String |
  MethodHandle U1 U2 |
  MethodType U2 |
  InvokeDynamic U2 U2
  deriving (Eq, Show)

instance Decode DecodeState Constant where
  decoder = do
    tag <- decoder :: Decoder U1
    case tag of
      7  -> liftM  Class decoder
      9  -> liftM2 Field decoder decoder
      10 -> liftM2 Method decoder decoder
      11 -> liftM2 InterfaceMethod decoder decoder
      8  -> liftM  String decoder
      3  -> liftM  Integer decoder
      4  -> liftM  Float decoder
      5  -> liftM  Long decoder
      6  -> liftM  Double decoder
      12 -> liftM2 NameAndType decoder decoder
      1  -> do
        len <- decoder :: Decoder U2
        bytes <- byteStringDecoder $ fromIntegral len
        maybe
          (fail "invalid utf8")
          (return . Utf8)
          (decodeString bytes)
      15 -> liftM2 MethodHandle decoder decoder
      16 -> liftM  MethodType decoder
      18 -> liftM2 InvokeDynamic decoder decoder
      _  -> fail "unexpected constant tag"

instance Encode EncodeState Constant where
  encoder (Class nameRef) = constant1encoder 7 nameRef
  encoder (Field classRef nameAndTypeRef) = constant2encoder 9 classRef nameAndTypeRef
  encoder (Method classRef nameAndTypeRef) = constant2encoder 10 classRef nameAndTypeRef
  encoder (InterfaceMethod classRef nameAndTypeRef) = constant2encoder 11 classRef nameAndTypeRef
  encoder (String utf8Ref) = constant1encoder 8 utf8Ref
  encoder (Integer bytes) = constant1encoder 3 bytes
  encoder (Float bytes) = constant1encoder 4 bytes
  encoder (Long bytes) = constant1encoder 5 bytes
  encoder (Double bytes) = constant1encoder 6 bytes
  encoder (NameAndType nameRef descriptorRef) = constant2encoder 12 nameRef descriptorRef
  encoder (Utf8 string) = constant1encoder 1 len >> byteStringEncoder bytes
    where
      bytes = encodeString string
      len = fromIntegral $ B.length bytes :: U2
  encoder (MethodHandle kind index) = constant2encoder 15 kind index
  encoder (MethodType descriptorRef) = constant1encoder 16 descriptorRef
  encoder (InvokeDynamic bootstrapMethodRef nameAndTypeRef) = constant2encoder 18 bootstrapMethodRef nameAndTypeRef

constant1encoder :: Encode EncodeState a => U1 -> a -> Encoder
constant1encoder tag field1 = encoder tag >> encoder field1

constant2encoder :: (Encode EncodeState a, Encode EncodeState b) => U1 -> a -> b -> Encoder
constant2encoder tag field1 field2 = encoder tag >> encoder field1 >> encoder field2
