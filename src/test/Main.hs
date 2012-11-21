module Main
       ( main
       ) where

import qualified Language.Java.Binary.Test as Binary

import           Test.Framework

tests =
  [ Binary.classTest
  , Binary.constantTest
  , Binary.primitiveTest
  , Binary.wordTest ]

main = defaultMain tests
