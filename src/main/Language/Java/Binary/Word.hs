{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Binary.Word
       ( U1 (..)
       , U2 (..)
       , U4 (..)
       , U8 (..)
       ) where

import Data.Binary

newtype U1 = U1 Word8  deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype U2 = U2 Word16 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype U4 = U4 Word32 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype U8 = U8 Word64 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)
