{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Language.Java.Binary.Test
       ( classTest
       , constantTest
       , primitiveTest
       , wordTest
       ) where

import           Language.Java.Binary.Class
import           Language.Java.Binary.Constant
import           Language.Java.Binary.Primitive
import           Language.Java.Binary.Word

import           Control.Applicative
import           Data.Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

binaryInverseProperty :: (Binary a, Eq a) => a -> Bool
binaryInverseProperty a = (decode $ encode a) == a

{- Binary.Class -}

instance Arbitrary Version where
  arbitrary = Version <$> arbitrary <*> arbitrary

deriving instance Arbitrary Flags

instance Arbitrary Attribute where
  arbitrary = Attribute <$> arbitrary <*> arbitraryByteString

instance Arbitrary Member where
  arbitrary = Member <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitraryVector

instance Arbitrary Class where
  arbitrary = Class <$> arbitrary <*> arbitraryVector <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitraryVector <*> arbitraryVector <*> arbitraryVector <*> arbitraryVector

arbitraryByteString :: Gen ByteString
arbitraryByteString = ByteString.pack <$> arbitrary

arbitraryVector :: Arbitrary a => Gen (Vector a)
arbitraryVector = Vector.fromList <$> arbitrary

classTest = testGroup
  "Binary inverse property for Binary.Class types"
  [ testProperty "Version"   (binaryInverseProperty :: Version   -> Bool)
  , testProperty "Flags"     (binaryInverseProperty :: Flags     -> Bool)
  , testProperty "Attribute" (binaryInverseProperty :: Attribute -> Bool)
  , testProperty "Member"    (binaryInverseProperty :: Member    -> Bool)
  , testProperty "Class"     (binaryInverseProperty :: Class     -> Bool)
  ]

{- Binary.Constant -}

instance Arbitrary MethodHandleKind where
  arbitrary = elements
    [ GetField
    , GetStatic
    , PutField
    , PutStatic
    , InvokeVirtual
    , InvokeStatic
    , InvokeSpecial
    , NewInvokeSpecial
    , InvokeInterface
    ]

instance Arbitrary Constant where
  arbitrary = oneof
    [ ClassConstant <$> arbitrary 
    , FieldConstant <$> arbitrary <*> arbitrary
    , MethodConstant <$> arbitrary <*> arbitrary
    , InterfaceMethodConstant <$> arbitrary <*> arbitrary
    , StringConstant <$> arbitrary
    , IntegerConstant <$> arbitrary
    , FloatConstant <$> arbitrary
    , LongConstant <$> arbitrary
    , DoubleConstant <$> arbitrary
    , NameAndTypeConstant <$> arbitrary <*> arbitrary
    , Utf8Constant <$> arbitrary
    , MethodHandleConstant <$> arbitrary <*> arbitrary
    , MethodTypeConstant <$> arbitrary
    , InvokeDynamicConstant <$> arbitrary <*> arbitrary
    ]

constantTest = testGroup
  "Binary inverse property for Binary.Constant types"
  [ testProperty "MethodHandleKind" (binaryInverseProperty :: MethodHandleKind -> Bool)
  , testProperty "Constant"         (binaryInverseProperty :: Constant         -> Bool)
  ]

{- Binary.Primitive -}

deriving instance Arbitrary JDouble
deriving instance Arbitrary JFloat
deriving instance Arbitrary JInt
deriving instance Arbitrary JLong
deriving instance Arbitrary JString


primitiveTest = testGroup
  "Binary inverse property for Binary.Primitive types"
  [ testProperty "JDouble" (binaryInverseProperty :: JDouble -> Bool)
  , testProperty "JFloat"  (binaryInverseProperty :: JFloat  -> Bool)
  , testProperty "JInt"    (binaryInverseProperty :: JInt    -> Bool)
  , testProperty "JLong"   (binaryInverseProperty :: JLong   -> Bool)
  , testProperty "JString" (binaryInverseProperty :: JString -> Bool)
  ]

{- Binary.Word -}

deriving instance Arbitrary U1
deriving instance Arbitrary U2
deriving instance Arbitrary U4
deriving instance Arbitrary U8

wordTest = testGroup
  "Binary inverse property for Binary.Word types"
  [ testProperty "U1" (binaryInverseProperty :: U1 -> Bool)
  , testProperty "U2" (binaryInverseProperty :: U2 -> Bool)
  , testProperty "U4" (binaryInverseProperty :: U4 -> Bool)
  , testProperty "U8" (binaryInverseProperty :: U8 -> Bool)
  ]