{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens.TH
import Control.Lens.Combinators
import Robot as R
import Table
import Type as T


data Game = Game { _table :: Table } deriving (Show)

makeLenses ''Game

initTable w h = Table {
  _width = w
  , _height = h
  , _robot = R.NotPlacedRobot
}

initGame = Game { _table = initTable 5 5 }

executeCommand :: T.Command -> Game -> Game
executeCommand T.Left = over (table . robot) R.turnLeft
executeCommand T.Right = over (table . robot) R.turnRight
executeCommand (T.Place x y d) = set (table . robot) $ Robot x y d
executeCommand T.Move = over (table . robot) R.move
executeCommand T.Report = id

report :: Game -> String
report game = show $ view (table . robot) game
