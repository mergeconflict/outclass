{-# LANGUAGE TypeSynonymInstances #-}

module Language.Java.Codec
       ( Codec (..)
       , decode
       , encode
       ) where

import           Language.Java.Constants
import           Language.Java.Codec.Bytes
import           Language.Java.Codec.Constants.Utf8
import qualified Language.Java.Codec.Decoder as D (State, Refs (..))
import           Language.Java.Codec.Decoder
import qualified Language.Java.Codec.Encoder as E (State, Refs (..))
import           Language.Java.Codec.Encoder

import           Control.Applicative
import           Control.Monad.RWS
import           Data.ByteString.Lazy
import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Typeable

class Codec a where
  dec :: Decoder a
  enc :: a -> Encoder

decode :: Codec a => D.State -> ByteString -> a
decode s b = runDecoder dec s b

encode :: Codec a => E.State -> a -> ByteString
encode s b = runEncoder (enc b) s

{- Binary -}

instance Codec U1     where dec = binaryDecoder; enc = binaryEncoder
instance Codec U2     where dec = binaryDecoder; enc = binaryEncoder
instance Codec U4     where dec = binaryDecoder; enc = binaryEncoder
instance Codec U8     where dec = binaryDecoder; enc = binaryEncoder
instance Codec Int32  where dec = binaryDecoder; enc = binaryEncoder
instance Codec Float  where dec = binaryDecoder; enc = binaryEncoder
instance Codec Int64  where dec = binaryDecoder; enc = binaryEncoder
instance Codec Double where dec = binaryDecoder; enc = binaryEncoder

{- References -}

-- TODO don't use (!) for map search, use lookup and fail in the appropriate monad

decU2Ref :: Typeable a => (D.Refs -> HashMap TypeRep (IntMap a)) -> Decoder a
decU2Ref f = do
  ref <- fromIntegral <$> (dec :: Decoder U2)
  tm  <- f <$> ask
  let t = typeOf a
      m = tm HashMap.! t
      a = m IntMap.! ref
  return a

encU2Ref :: (Eq a, Hashable a, Typeable a) => (E.Refs -> HashMap TypeRep (HashMap a U2)) -> a -> Encoder
encU2Ref f a = do
  tm <- f <$> ask
  let t   = typeOf a
      m   = tm HashMap.! t
      ref = m HashMap.! a
  enc ref

{- Constants -}

-- TODO maintain state, tell the appropriate writer what's been visited

decConstantRef :: Typeable a => Decoder a
decConstantRef = decU2Ref D.constants

encConstantRef :: (Eq a, Hashable a, Typeable a) => a -> Encoder
encConstantRef = encU2Ref E.constants

decConstant1 :: Typeable a => (a -> b) -> Decoder b
decConstant1 f = f <$> decConstantRef

decConstant2 :: (Typeable a, Typeable b) => (a -> b -> c) -> Decoder c
decConstant2 f = f <$> decConstantRef <*> decConstantRef

encConstant1 :: (Eq a, Hashable a, Typeable a) => a -> Encoder
encConstant1 a = encConstantRef a

encConstant2 :: (Eq a, Eq b, Hashable a, Hashable b, Typeable a, Typeable b) => a -> b -> Encoder
encConstant2 a b = encConstantRef a >> encConstantRef b

instance Codec ClassC where
  dec = decConstant1 Class
  enc (Class n) = encConstant1 n

instance Codec FieldC where
  dec = decConstant2 Field
  enc (Field c nt) = encConstant2 c nt

instance Codec MethodC where
  dec = decConstant2 Method
  enc (Method c nt) = encConstant2 c nt

instance Codec InterfaceMethodC where
  dec = decConstant2 InterfaceMethod
  enc (InterfaceMethod c nt) = encConstant2 c nt

instance Codec StringC where
  dec = decConstant1 String
  enc (String u) = encConstant1 u

instance Codec IntegerC where
  dec = Integer <$> dec
  enc (Integer i) = enc i

instance Codec FloatC where
  dec = Float <$> dec
  enc (Float f) = enc f

instance Codec LongC where
  dec = Long <$> dec
  enc (Long l) = enc l

instance Codec DoubleC where
  dec = Double <$> dec
  enc (Double d) = enc d

instance Codec NameAndTypeC where
  dec = decConstant2 NameAndType
  enc (NameAndType n t) = encConstant2 n t

instance Codec Utf8C where
  dec = do
    l <- fromIntegral <$> (dec :: Decoder U2)
    s <- decodeString <$> byteStringDecoder l
    maybe (fail "invalid utf-8 constant") (return . Utf8) s
  enc (Utf8 s) = byteStringEncoder $ encodeString s

instance Codec MethodHandleC where
  dec = undefined -- TODO
  enc = undefined -- TODO

instance Codec MethodTypeC where
  dec = decConstant1 MethodType
  enc (MethodType s) = encConstant1 s

instance Codec InvokeDynamicC where
  dec = undefined -- TODO
  enc = undefined -- TODO

encConstant :: Codec a => U1 -> a -> Encoder
encConstant t a = enc t >> enc a

instance Codec Constant where
  dec = do
    t <- dec :: Decoder U1
    case t of
      7  -> ClassConstant           <$> dec
      9  -> FieldConstant           <$> dec
      10 -> MethodConstant          <$> dec
      11 -> InterfaceMethodConstant <$> dec
      8  -> StringConstant          <$> dec
      3  -> IntegerConstant         <$> dec
      4  -> FloatConstant           <$> dec
      5  -> LongConstant            <$> dec
      6  -> DoubleConstant          <$> dec
      12 -> NameAndTypeConstant     <$> dec
      1  -> Utf8Constant            <$> dec
      15 -> MethodHandleConstant    <$> dec
      16 -> MethodTypeConstant      <$> dec
      18 -> InvokeDynamicConstant   <$> dec
      _  -> fail "invalid constant tag"
  enc (ClassConstant c)           = encConstant 7  c
  enc (FieldConstant c)           = encConstant 9  c
  enc (MethodConstant c)          = encConstant 10 c
  enc (InterfaceMethodConstant c) = encConstant 11 c
  enc (StringConstant c)          = encConstant 8  c
  enc (IntegerConstant c)         = encConstant 3  c
  enc (FloatConstant c)           = encConstant 4  c
  enc (LongConstant c)            = encConstant 5  c
  enc (DoubleConstant c)          = encConstant 6  c
  enc (NameAndTypeConstant c)     = encConstant 12 c
  enc (Utf8Constant c)            = encConstant 1  c
  enc (MethodHandleConstant c)    = encConstant 15 c
  enc (MethodTypeConstant c)      = encConstant 16 c
  enc (InvokeDynamicConstant c)   = encConstant 18 c