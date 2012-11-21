{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Java.Binary.Class
	     ( Class (..)
       , Field (..)
       , Method (..)
	     ) where

import Language.Java.Binary.Bytes
import Language.Java.Binary.Constants

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as Strict

{- version -}

data Version = Version U2 U2 deriving Show

instance Binary Version where
  get = get2 Version
  put (Version minor major) = put2 minor major

{- flags -}

newtype Flags = Flags U2 deriving (Binary, Show)

{- attribute -}

data Attribute = Attribute U2 Strict.ByteString deriving Show

instance Binary Attribute where
  get = do
    ref  <- get
    len  <- get :: Get U4
    info <- getByteString $ fromIntegral len
    return $ Attribute ref info
  put (Attribute ref info) = do
    put ref
    put $ Strict.length info
    putByteString info

{- field / method -}

data Field = Field Flags U2 U2 [Attribute] deriving Show

instance Binary Field where
  get = getFieldOrMethod Field
  put (Field flags nameRef descriptorRef attributes) =
    putFieldOrMethod flags nameRef descriptorRef attributes

data Method = Method Flags U2 U2 [Attribute] deriving Show

instance Binary Method where
  get = getFieldOrMethod Method
  put (Method flags nameRef descriptorRef attributes) =
    putFieldOrMethod flags nameRef descriptorRef attributes

getFieldOrMethod :: (Flags -> U2 -> U2 -> [Attribute] -> a) -> Get a
getFieldOrMethod f =
  f <$> get <*> get <*> get <*> getList

putFieldOrMethod :: Flags -> U2 -> U2 -> [Attribute] -> Put
putFieldOrMethod flags nameRef descriptorRef attributes = do
  put flags
  put nameRef
  put descriptorRef
  putList attributes

{- class -}

data Class = Class Version Constants Flags U2 U2 [U2] [Field] [Method] [Attribute] deriving Show

instance Binary Class where
  get = do
    magic <- get :: Get U4
    if magic /= 0xCAFEBABE
      then fail "invalid magic"
      else Class <$> get <*> get <*> get <*> get <*> get <*> getList <*> getList <*> getList <*> getList
  put (Class version constants flags this super interfaces fields methods attributes) = do
    put (0xCAFEBABE :: U4)
    put version
    put constants
    put flags
    put this
    put super
    putList interfaces
    putList fields
    putList methods
    putList attributes