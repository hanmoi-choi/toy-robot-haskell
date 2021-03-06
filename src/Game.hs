{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens.TH
import Control.Lens.Combinators
import Robot as R
import Table
import Type as T
import Control.Monad.Trans.State

data Game = Game { _table :: Table } deriving (Show)

makeLenses ''Game

type GameState = State Game String
type IOGameState = StateT Game IO String

initTable :: Integer -> Integer -> Table
initTable w h = Table {
  _width = w
  , _height = h
  , _robot = R.NotPlacedRobot
}

initGame :: Game
initGame = Game { _table = initTable 5 5 }

executeCommand :: T.Command -> Game -> Game
executeCommand T.Left = over (table . robot) R.turnLeft
executeCommand T.Right = over (table . robot) R.turnRight
executeCommand (T.Place x y d) = set (table . robot) $ Robot x y d
executeCommand T.Move = over (table . robot) R.move
executeCommand T.Report = id

report :: Game -> String
report game = show $ view (table . robot) game

execute :: T.Command -> GameState
execute T.Report = report <$> get
execute command = do
  game <- get
  let newGame = executeCommand command game
  put newGame
  return ""

executeIO :: T.Command -> IOGameState
executeIO T.Report = report <$> get
executeIO command = do
  game <- get
  let newGame = executeCommand command game
  put newGame
  return ""
