module Language.Java.Codec.Class
	     ( Class (..)
	     ) where

import Language.Java.Codec.Constants

data Class = Class {
  constants :: [Constant]
} deriving Eq