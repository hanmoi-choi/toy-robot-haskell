{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Robot where

import Control.Lens.TH
import Control.Lens.Combinators
import qualified Type as T

data Robot = Robot { _xPos :: Int
                   , _yPos :: Int
                   , _direction :: T.Direction
                   } deriving (Eq)

instance Show Robot where
  show r = foldr (\e a -> a ++ "," ++ e) "" asString
    where
      toString = [show . _xPos, show . _yPos, show . _direction]
      asString = map (\f -> f r) toString
  --show (_xPos r) ++ "," ++ show (_yPos r) ++ "," ++ show (_direction r)

makeLenses ''Robot

orient :: T.Turn -> T.Direction -> T.Direction
orient T.TNone = id
orient T.TLeft = T.cpred
orient T.TRight = T.csucc

-- robot = Robot 1 1 North
-- Lense get => view direction robot
-- Lense update => over direction (orient TLeft) robot
