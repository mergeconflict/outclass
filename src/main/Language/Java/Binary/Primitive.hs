{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Binary.Primitive
       ( JDouble (..)
       , JFloat (..)
       , JInt (..)
       , JLong (..)
       , JString (..)
       ) where

import Language.Java.Binary.Word

import           Data.Bits
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as ByteString
import           Data.Char
import           Data.Int

newtype JInt    = JInt    Int32  deriving (Binary, Eq, Show)
newtype JLong   = JLong   Int64  deriving (Binary, Eq, Show)
newtype JFloat  = JFloat  Float  deriving (Binary, Eq, Show)
newtype JDouble = JDouble Double deriving (Binary, Eq, Show)

newtype JString = JString String deriving (Eq, Show)

instance Binary JString where
  get = do
    len <- get :: Get U2
    bytes <- getByteString $ fromIntegral len 
    go [] $ map fromIntegral $ ByteString.unpack bytes
      where
        go acc (0xED : v : w : 0xED : y : z : bs)
          | v `shiftR` 4 == 0x0A &&
            w `shiftR` 6 == 0x02 &&
            y `shiftR` 4 == 0x0B &&
            z `shiftR` 6 == 0x02 = go (c:acc) bs
              where c = chr $
                        0x10000 +
                        (v .&. 0x0F) `shiftL` 16 +
                        (w .&. 0x3F) `shiftL` 10 +
                        (y .&. 0x0F) `shiftL` 6 +
                        (z .&. 0x3F)
        go acc (x : y : z : bs)
          | x `shiftR` 4 == 0x0E &&
            y `shiftR` 6 == 0x02 &&
            z `shiftR` 6 == 0x02 = go (c:acc) bs
              where c = chr $
                        (x .&. 0x0F) `shiftL` 12 +
                        (y .&. 0x3F) `shiftL` 6 +
                        (z .&. 0x3F)
        go acc (x : y : bs)
          | x `shiftR` 5 == 0x06 &&
            y `shiftR` 6 == 0x02 = go (c:acc) bs
              where c = chr $
                        (x .&. 0x1F) `shiftL` 6 +
                        (y .&. 0x3F)
        go acc (x : bs)
          | x `shiftR` 7 == 0x00 = go (c:acc) bs
              where c = chr $ x
        go acc [] = return $ JString $ reverse acc
        go _ _ = fail "invalid JString"

  put (JString s) = do
    put len
    putByteString bytes
      where
        bytes = ByteString.pack $ map fromIntegral $ go . ord =<< s
        len   = fromIntegral $ ByteString.length bytes :: U2
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
