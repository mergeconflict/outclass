{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Binary.State
       ( TellM
       , Tell
       , Ask
       , Binary(..)
       , tellWord8
       , askWord8
       ) where

import           Control.Monad.State.Strict (lift)
import qualified Data.Binary as B (Binary, get, put)
import           Data.Binary.State.Tell
import           Data.Binary.State.Ask

class Binary s a where
  tell :: a -> Tell s
  ask :: Ask s a

instance B.Binary b => Binary s b where
  tell = lift . B.put
  ask = lift B.get