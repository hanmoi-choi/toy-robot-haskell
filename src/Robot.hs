{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Robot where

import Data.List as L
import Control.Lens.TH
import Control.Lens.Combinators
import qualified Type as T

data Robot =
  NotPlacedRobot
  | Robot { _xPos :: Integer
            , _yPos :: Integer
            , _direction :: T.Direction
          } deriving (Eq)

instance Show Robot where
  show NotPlacedRobot = "Place Robot First"
  show r = L.intercalate "," asString
    where
      toString = [show . _xPos, show . _yPos, show . _direction]
      asString = map (\f -> f r) toString

makeLenses ''Robot

orient :: T.Turn -> T.Direction -> T.Direction
orient T.TNone = id
orient T.TLeft = T.cpred
orient T.TRight = T.csucc

turnLeft :: Robot -> Robot
turnLeft NotPlacedRobot = NotPlacedRobot
turnLeft robot = over direction (orient T.TLeft) robot

turnRight :: Robot -> Robot
turnRight NotPlacedRobot = NotPlacedRobot
turnRight robot = over direction (orient T.TRight) robot

move :: Robot -> Robot
move NotPlacedRobot = NotPlacedRobot
move (Robot x y d) = case d of
          T.North -> Robot x (y + 1) d
          T.South -> Robot x (y - 1) d
          T.East -> Robot (x + 1) y d
          T.West -> Robot (x - 1) y d
