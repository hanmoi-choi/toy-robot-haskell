{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Table where

import Control.Lens.TH
import Control.Lens.Combinators

import Robot

data Table = Table { _width  :: Integer
                   , _height :: Integer
                   , _robot :: Robot
                   } deriving (Show, Eq)

makeLenses ''Table

-- over (robot . direction) (orient TLeft) nt
