{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Language.Java.Binary.Class
	     ( Attribute (..)
       , Class (..)
       , Flags (..)
       , Member (..)
       , Version (..)
	     ) where

import Language.Java.Binary.Constant
import Language.Java.Binary.Word

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

{- version -}

data Version = Version { minorVersion :: U2, majorVersion :: U2 } deriving (Eq, Show)

instance Binary Version where
  get = Version <$> get <*> get
  put (Version minorVersion majorVersion) =
    put minorVersion >> put majorVersion

{- flags -}

newtype Flags = Flags U2 deriving (Binary, Eq, Show)

{- attribute -}

data Attribute = Attribute { attributeNameIndex :: U2, attributeInfo :: ByteString } deriving (Eq, Show)

instance Binary Attribute where
  get = do
    nameIndex       <- get
    attributeLength <- get :: Get U4
    attributeInfo   <- getByteString $ fromIntegral attributeLength
    return $ Attribute nameIndex attributeInfo
  put (Attribute nameIndex attributeInfo) =
    put nameIndex >> put (fromIntegral $ ByteString.length attributeInfo :: U4) >> putByteString attributeInfo

{- member -}

data Member = Member { memberFlags :: Flags, memberNameIndex :: U2, descriptorIndex :: U2, memberAttributes :: Vector Attribute } deriving (Eq, Show)

instance Binary Member where
  get = Member <$> get <*> get <*> get <*> getVector
  put (Member flags nameIndex descriptorIndex attributes) =
    put flags >> put nameIndex >> put descriptorIndex >> putVector attributes

{- class -}

data Class = Class
  { version :: Version
  , constants :: Vector Constant
  , classFlags :: Flags
  , thisClass :: U2
  , superClass :: U2
  , interfaces :: Vector U2
  , fields :: Vector Member
  , methods :: Vector Member
  , classAttributes :: Vector Attribute
  } deriving (Eq, Show)

instance Binary Class where
  get = do
    magic <- get :: Get U4
    if magic /= 0xCAFEBABE
      then fail "invalid magic"
      else Class <$> get <*> getVector1 <*> get <*> get <*> get <*> getVector <*> getVector <*> getVector <*> getVector
  put (Class version constants flags this super interfaces fields methods attributes) = do
    put (0xCAFEBABE :: U4)
    put version
    putVector1 constants
    put flags
    put this
    put super
    putVector interfaces
    putVector fields
    putVector methods
    putVector attributes

{- helpers -}

getVector :: Binary a => Get (Vector a)
getVector = do
  len <- get :: Get U2
  Vector.replicateM (fromIntegral len) get

putVector :: Binary a => Vector a -> Put
putVector v =
  put (fromIntegral $ Vector.length v :: U2) >> Vector.mapM_ put v

getVector1 :: Binary a => Get (Vector a)
getVector1 = do
  len <- get :: Get U2
  Vector.replicateM (fromIntegral $ len - 1) get

putVector1 :: Binary a => Vector a -> Put
putVector1 v =
  put (fromIntegral $ Vector.length v + 1 :: U2) >> Vector.mapM_ put v
