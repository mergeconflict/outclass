{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Primitives
       ( JBoolean (..)
       , JByte (..)
       , JChar (..)
       , JDouble (..)
       , JFloat (..)
       , JInt (..)
       , JLong (..)
       , JShort (..)
       ) where

import Data.Binary
import Data.Bits
import Data.Int

newtype JByte    = JByte Int8
  deriving (Binary, Bits, Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype JShort   = JShort Int16
  deriving (Binary, Bits, Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype JInt     = JInt Int32
  deriving (Binary, Bits, Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype JLong    = JLong Int64
  deriving (Binary, Bits, Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype JFloat   = JFloat Float
  deriving (Binary, Eq, Floating, Fractional, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

newtype JDouble  = JDouble Double
  deriving (Binary, Eq, Floating, Fractional, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

newtype JBoolean = JBoolean Bool
  deriving (Binary, Bounded, Enum, Eq, Ord, Read, Show)

newtype JChar    = JChar Char -- TODO forbid anything larger than 16 bit
  deriving (Binary, Bounded, Enum, Eq, Ord, Read, Show)
