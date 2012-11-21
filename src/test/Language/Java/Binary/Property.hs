module Language.Java.Binary.Property
       ( binaryInverseProperty
       ) where

import Data.Binary

binaryInverseProperty :: (Binary a, Eq a) => a -> Bool
binaryInverseProperty a = (decode $ encode a) == a