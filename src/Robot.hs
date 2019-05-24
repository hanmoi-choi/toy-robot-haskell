{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Robot where

import Data.List as L
import Control.Lens.TH
import Control.Lens.Combinators
import qualified Type as T

{-
Personally I think a Robot does not have a concept of being placed or not.
Rather the table has a concept of having a robot placed ON it or not.
To me this is the concept of a table bleeding into the robot logic.
-}
data Robot =
  NotPlacedRobot
  | Robot { _xPos :: Integer
            , _yPos :: Integer
            , _direction :: T.Direction
          } deriving (Eq)

-- This is more technically incorrect stuff, where using Show is nice, but the
-- actual definition of Show is `The result of show is a syntactically correct
-- Haskell expression containing only constants` In other words, it needs to
-- output something like valid Haskell syntax as a String eg Robot 1 2 North .
-- The rule is that read . show = id
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
