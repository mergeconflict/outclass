{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Codec.Constants
       ( MethodHandleKind (..)
       , Constant (..)
       , Constants
       , (!?)
       ) where

import Language.Java.Codec.Bytes

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString as Strict
import           Data.Char
import           Data.Int
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

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
  deriving Show

instance Binary MethodHandleKind where
  get = do
    kind <- getTag
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
      _ -> fail "invalid method handle kind"
  put kind =
    putTag $ case kind of
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
  ClassConstant            U2                  |
  FieldConstant            U2               U2 |
  MethodConstant           U2               U2 |
  InterfaceMethodConstant  U2               U2 |
  StringConstant           U2                  |
  IntegerConstant          Int32               |
  FloatConstant            Float               |
  LongConstant             Int64               |
  DoubleConstant           Double              |
  NameAndTypeConstant      U2               U2 |
  Utf8Constant             String              |
  MethodHandleConstant     MethodHandleKind U2 |
  MethodTypeConstant       U2                  |
  InvokeDynamicConstant    U2               U2
  deriving Show

instance Binary Constant where
  get = do
    tag <- getTag
    case tag of
      7  -> get1 ClassConstant
      9  -> get2 FieldConstant
      10 -> get2 MethodConstant
      11 -> get2 InterfaceMethodConstant
      8  -> get1 StringConstant
      3  -> get1 IntegerConstant
      4  -> get1 FloatConstant
      5  -> get1 LongConstant
      6  -> get1 DoubleConstant
      12 -> get2 NameAndTypeConstant
      1  -> do
              len <- get :: Get U2
              str <- getByteString $ fromIntegral len
              case decodeString str of
                Just u  -> return $ Utf8Constant u
                Nothing -> fail "invalid utf-8 constant"
      15 -> get2 MethodHandleConstant
      16 -> get1 MethodTypeConstant
      18 -> get2 InvokeDynamicConstant
      _  -> fail "invalid constant tag"
  put (ClassConstant           nameRef)                           = putTag 7  >> put1 nameRef
  put (FieldConstant           classRef           nameAndTypeRef) = putTag 9  >> put2 classRef nameAndTypeRef
  put (MethodConstant          classRef           nameAndTypeRef) = putTag 10 >> put2 classRef nameAndTypeRef
  put (InterfaceMethodConstant classRef           nameAndTypeRef) = putTag 11 >> put2 classRef nameAndTypeRef
  put (StringConstant          utf8Ref)                           = putTag 8  >> put1 utf8Ref
  put (IntegerConstant         value)                             = putTag 3  >> put1 value
  put (FloatConstant           value)                             = putTag 4  >> put1 value
  put (LongConstant            value)                             = putTag 5  >> put1 value
  put (DoubleConstant          value)                             = putTag 6  >> put1 value
  put (NameAndTypeConstant     nameRef            descriptorRef)  = putTag 12 >> put2 nameRef descriptorRef
  put (Utf8Constant            value)                             = putTag 1  >> put1 len >> putByteString bytes
                                                              where
                                                                bytes = encodeString value
                                                                len   = fromIntegral $ Strict.length bytes :: U2
  put (MethodHandleConstant    kind               ref)            = putTag 15 >> put2 kind ref
  put (MethodTypeConstant      descriptorRef)                     = putTag 16 >> put1 descriptorRef
  put (InvokeDynamicConstant   bootstrapMethodRef nameAndTypeRef) = putTag 18 >> put2 bootstrapMethodRef nameAndTypeRef

newtype Constants = Constants (Vector Constant) deriving Show

instance Binary Constants where
  get = do
    len <- get :: Get U2
    cs  <- Vector.replicateM (fromIntegral $ len - 1) get
    return $ Constants cs
  put (Constants cs) = do
    let len = Vector.length cs
    put (fromIntegral $ len + 1 :: U2)
    Vector.mapM_ put cs

(!?) :: Constants -> Int -> Maybe Constant
(Constants cs) !? n = cs Vector.!? (n + 1)

{- utf8 -}

encodeString :: String -> Strict.ByteString
encodeString = Strict.concat . map (Strict.pack . encodeChar)
  where
    encodeChar :: Char -> [Word8]
    encodeChar = map fromIntegral . go . ord

    go :: Int -> [Int]
    go b
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

decodeString :: Strict.ByteString -> Maybe String
decodeString = fmap reverse . go "" . map fromIntegral . Strict.unpack
  where
    go :: String -> [Int] -> Maybe String
    go s (0xED : v : w : 0xED : y : z : bs)
      | v `shiftR` 4 == 0x0A &&
        w `shiftR` 6 == 0x02 &&
        y `shiftR` 4 == 0x0B &&
        z `shiftR` 6 == 0x02 = go (c:s) bs
          where c = chr $
                    0x10000 +
                    (v .&. 0x0F) `shiftL` 16 +
                    (w .&. 0x3F) `shiftL` 10 +
                    (y .&. 0x0F) `shiftL` 6 +
                    (z .&. 0x3F)
    go s (x : y : z : bs)
      | x `shiftR` 4 == 0x0E &&
        y `shiftR` 6 == 0x02 &&
        z `shiftR` 6 == 0x02 = go (c:s) bs
          where c = chr $
                    (x .&. 0x0F) `shiftL` 12 +
                    (y .&. 0x3F) `shiftL` 6 +
                    (z .&. 0x3F)
    go s (x : y : bs)
      | x `shiftR` 5 == 0x06 &&
        y `shiftR` 6 == 0x02 = go (c:s) bs
          where c = chr $
                    (x .&. 0x1F) `shiftL` 6 +
                    (y .&. 0x3F)
    go s (x : bs)
      | x `shiftR` 7 == 0x00 = go (c:s) bs
          where c = chr $ x
    go s [] = Just s
    go _ _ = Nothing
