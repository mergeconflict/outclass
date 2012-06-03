module Main
       ( main
       ) where

import Data.Binary (Binary, decode, encode)
import Control.Monad (liftM, liftM2, unless)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, quickCheckResult)
import Test.QuickCheck.Test (isSuccess)
import System.Exit (exitFailure)

import Language.Java.Constant

instance Arbitrary Constant where
  arbitrary = oneof
              [ liftM  Class arbitrary 
              , liftM2 Field arbitrary arbitrary
              , liftM2 Method arbitrary arbitrary
              , liftM2 InterfaceMethod arbitrary arbitrary
              , liftM  String arbitrary
              , liftM  Integer arbitrary
              , liftM  Float arbitrary
              , liftM  Long arbitrary
              , liftM  Double arbitrary
              , liftM2 NameAndType arbitrary arbitrary
              , liftM  Utf8 arbitrary
              , liftM2 MethodHandle arbitrary arbitrary
              , liftM  MethodType arbitrary
              , liftM2 InvokeDynamic arbitrary arbitrary
              ]

roundTrip :: (Binary a, Eq a) => a -> Bool
roundTrip b = (decode $ encode b) == b

main = do
  result <- quickCheckResult (roundTrip :: Constant -> Bool)
  unless (isSuccess result) exitFailure