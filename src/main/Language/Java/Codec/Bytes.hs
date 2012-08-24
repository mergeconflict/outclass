{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Codec.Bytes
       ( U1
       , U2
       , U4
       , U8
       , getTag
       , getList
       , get1
       , get2
       , putTag
       , putList
       , put1
       , put2
       ) where

import Control.Applicative
import Control.Monad
import Data.Binary

newtype U1 = U1 Word8  deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype U2 = U2 Word16 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype U4 = U4 Word32 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype U8 = U8 Word64 deriving (Binary, Enum, Eq, Integral, Num, Ord, Real, Show)

{- helpers -}

getTag :: Get U1
getTag = get

getList :: Binary a => Get [a]
getList = do
  len <- get :: Get U2
  replicateM (fromIntegral len) get

get1 :: Binary a => (a -> b) -> Get b
get1 f = f <$> get

get2 :: (Binary a, Binary b) => (a -> b -> c) -> Get c
get2 f = f <$> get <*> get

putTag :: U1 -> Put
putTag = put

putList :: Binary a => [a] -> Put
putList as = do
  let len = length as
  put (fromIntegral len :: U2)
  mapM_ put as

put1 :: Binary a => a -> Put
put1 a = put a

put2 :: (Binary a, Binary b) => a -> b -> Put
put2 a b = put a >> put b
