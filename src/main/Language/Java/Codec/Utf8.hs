module Language.Java.Codec.Utf8
       ( encodeString
       , decodeString
       ) where

import Language.Java.Bytes

import           Data.Bits ((.|.), (.&.), shiftL, shiftR)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B (concat, pack, unpack)
import           Data.Char (chr, ord)

encodeString :: String -> ByteString
encodeString = B.concat . map (B.pack . encodeChar)
  where
    encodeChar :: Char -> [U1]
    encodeChar = map fromIntegral . encode . ord

    encode :: Int -> [Int]
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
