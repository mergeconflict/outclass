module Main
       ( main
       ) where

import qualified Language.Java.Binary.ConstantTest as Binary.ConstantTest
import qualified Language.Java.Binary.PrimitiveTest as Binary.PrimitiveTest
import qualified Language.Java.Binary.WordTest as Binary.WordTest

import           Test.Framework

tests =
  [ Binary.ConstantTest.test
  , Binary.PrimitiveTest.test
  , Binary.WordTest.test ]

main = defaultMain tests
