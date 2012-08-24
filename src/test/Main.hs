module Main
       ( main
       ) where

{-
import Language.Java.Codec.Constant
import Language.Java.Codec (DecodeState, EncodeState, decode, encode)

import Codec.Class (Decode, Encode)
import Control.Monad (liftM, liftM2, unless)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, verboseCheckResult)
import Test.QuickCheck.Test (isSuccess)
import System.Exit (exitFailure)

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

roundTrip :: (Decode DecodeState a, Encode EncodeState a, Eq a) => a -> Bool
roundTrip b = (decode $ encode b) == b

main = do
  result <- verboseCheckResult (roundTrip :: Constant -> Bool)
  unless (isSuccess result) exitFailure
-}

import Language.Java.Codec.Class

import Data.Binary
import System.Environment

main = do
  filename <- fmap head getArgs
  clazz    <- decodeFile filename :: IO Class
  putStrLn $ show clazz