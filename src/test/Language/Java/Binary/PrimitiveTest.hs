module Language.Java.Binary.PrimitiveTest
       ( test
       ) where

import Language.Java.Binary.Instance
import Language.Java.Binary.Primitive
import Language.Java.Binary.Property

import Test.Framework
import Test.Framework.Providers.QuickCheck2

test = testGroup
  "Binary inverse property for Binary.Primitive types"
  [ testProperty "JDouble" (binaryInverseProperty :: JDouble -> Bool)
  , testProperty "JFloat"  (binaryInverseProperty :: JFloat  -> Bool)
  , testProperty "JInt"    (binaryInverseProperty :: JInt    -> Bool)
  , testProperty "JLong"   (binaryInverseProperty :: JLong   -> Bool)
  , testProperty "JString" (binaryInverseProperty :: JString -> Bool)
  ]
