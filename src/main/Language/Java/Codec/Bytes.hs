{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Java.Codec.Bytes
       ( U1
       , U2
       , U4
       , U8
       ) where

import Data.Binary

newtype U1 = U1 Word8  deriving (Binary, Enum, Eq, Integral, Num, Ord, Real)
newtype U2 = U2 Word16 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real)
newtype U4 = U4 Word32 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real)
newtype U8 = U8 Word64 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real)