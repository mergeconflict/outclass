module Language.Java.Constant 
       ( Constant(..)
       ) where

import Language.Java.Bytes

import Control.Monad (liftM, liftM2)
import Data.Binary (Binary, Get, Put, get, put)
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (concat, length, pack, unpack)
import Data.Char (chr, ord)
import Data.Maybe ()

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

putC1 :: Binary a => U1 -> a -> Put
putC1 tag field1 = put tag >> put field1

putC2 :: (Binary a, Binary b) => U1 -> a -> b -> Put
putC2 tag field1 field2 = put tag >> put field1 >> put field2

instance Binary Constant where
  put (Class nameRef) = putC1 7 nameRef
  put (Field classRef nameAndTypeRef) = putC2 9 classRef nameAndTypeRef
  put (Method classRef nameAndTypeRef) = putC2 10 classRef nameAndTypeRef
  put (InterfaceMethod classRef nameAndTypeRef) = putC2 11 classRef nameAndTypeRef
  put (String utf8Ref) = putC1 8 utf8Ref
  put (Integer bytes) = putC1 3 bytes
  put (Float bytes) = putC1 4 bytes
  put (Long bytes) = putC1 5 bytes
  put (Double bytes) = putC1 6 bytes
  put (NameAndType nameRef descriptorRef) = putC2 12 nameRef descriptorRef
  put (Utf8 string) = putC1 1 len >> putByteString bytes
    where
      bytes = encodeString string
      len = fromIntegral $ B.length bytes :: U2
  put (MethodHandle kind index) = putC2 15 kind index
  put (MethodType descriptorRef) = putC1 16 descriptorRef
  put (InvokeDynamic bootstrapMethodRef nameAndTypeRef) = putC2 18 bootstrapMethodRef nameAndTypeRef

  get = do
    tag <- get :: Get U1
    case tag of
      7  -> liftM  Class get
      9  -> liftM2 Field get get
      10 -> liftM2 Method get get
      11 -> liftM2 InterfaceMethod get get
      8  -> liftM  String get
      3  -> liftM  Integer get
      4  -> liftM  Float get
      5  -> liftM  Long get
      6  -> liftM  Double get
      12 -> liftM2 NameAndType get get
      1  -> do
        len <- get :: Get U2
        bytes <- getByteString $ fromIntegral len
        maybe
          (fail "invalid utf8")
          (return . Utf8)
          (decodeString bytes)
      15 -> liftM2 MethodHandle get get
      16 -> liftM  MethodType get
      18 -> liftM2 InvokeDynamic get get
      _  -> fail "unexpected constant tag"

encodeChar :: Char -> [U1]
encodeChar = map fromIntegral . encode . ord
  where
    encode b
      | b == 0x0    = [0xC0, 0x80]
      | b < 0x80    = [b]
      | b < 0x800   = [b `shiftR` 6 .|. 0xC0,
                       b .&. 0x3F .|. 0x80]
      | b < 0x10000 = [b `shiftR` 12 .|. 0xE0,
                       b `shiftR` 6 .&. 0x3F .|. 0x80,
                       b .&. 0x3F .|. 0x80]
      | otherwise   = [0xED,
                       b `shiftR` 16 - 1 .|. 0xA0,
                       b `shiftR` 10 .&. 0x3F .|. 0x80,
                       0xED,
                       b `shiftR` 6 .&. 0x0F .|. 0xB0,
                       b .&. 0x3F .|. 0x80]

encodeString :: String -> ByteString
encodeString = B.concat . map (B.pack . encodeChar)

decodeString :: ByteString -> Maybe String
decodeString = fmap reverse . decode "" . map fromIntegral . B.unpack
  where
    decode :: String -> [Int] -> Maybe String
    decode s (0xED : v : w : 0xED : y : z : bs)
      | v `shiftR` 4 == 0x0A && 
        w `shiftR` 6 == 0x02 &&
        y `shiftR` 4 == 0x0B &&
        z `shiftR` 6 == 0x02 = decode (c:s) bs
          where c = chr $
                    0x10000 +
                    (v .&. 0x0F) `shiftL` 16 +
                    (w .&. 0x3F) `shiftL` 10 +
                    (y .&. 0x0F) `shiftL` 6 +
                    (z .&. 0x3F)
    decode s (x : y : z : bs)
      | x `shiftR` 4 == 0x0E &&
        y `shiftR` 6 == 0x02 &&
        z `shiftR` 6 == 0x02 = decode (c:s) bs
          where c = chr $
                    (x .&. 0x0F) `shiftL` 12 +
                    (y .&. 0x3F) `shiftL` 6 +
                    (z .&. 0x3F)
    decode s (x : y : bs)
      | x `shiftR` 5 == 0x06 &&
        y `shiftR` 6 == 0x02 = decode (c:s) bs
          where c = chr $
                    (x .&. 0x1F) `shiftL` 6 +
                    (y .&. 0x3F)
    decode s (x : bs)
      | x `shiftR` 7 == 0x00 = decode (c:s) bs
          where c = chr $ x
    decode s [] = Just s
    decode _ _ = Nothing