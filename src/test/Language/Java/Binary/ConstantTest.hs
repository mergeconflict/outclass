module Language.Java.Binary.ConstantTest
       ( test
       ) where

import Language.Java.Binary.Instance
import Language.Java.Binary.Constant
import Language.Java.Binary.Property

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

test = testGroup
  "Binary inverse property for Binary.Constant types"
  [ testProperty "MethodHandleKind" (binaryInverseProperty :: MethodHandleKind -> Bool)
  , testProperty "Constant"         (binaryInverseProperty :: Constant         -> Bool)
  ]
