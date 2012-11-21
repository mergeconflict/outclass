module Language.Java.Binary.WordTest
       ( test
       ) where

import Language.Java.Binary.Instance
import Language.Java.Binary.Property
import Language.Java.Binary.Word

import Test.Framework
import Test.Framework.Providers.QuickCheck2

test = testGroup
  "Binary inverse property for Binary.Word types"
  [ testProperty "U1" (binaryInverseProperty :: U1 -> Bool)
  , testProperty "U2" (binaryInverseProperty :: U2 -> Bool)
  , testProperty "U4" (binaryInverseProperty :: U4 -> Bool)
  , testProperty "U8" (binaryInverseProperty :: U8 -> Bool)
  ]