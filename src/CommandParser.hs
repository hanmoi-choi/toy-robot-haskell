module CommandParser where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

import Type as T

skipWhitespaces :: Parser ()
skipWhitespaces = skipMany (char ' ' <|> char '\n')

-- slightly more succinct is using $> from Data.Functor, which doesn't require a `f b`, just a `b`
-- eg. `string "north" $> T.North`
parseNorth :: Parser T.Direction
parseNorth = string "north" *> return T.North

parseSouth :: Parser T.Direction
parseSouth = string "south" *> return T.South

parseEast :: Parser T.Direction
parseEast = string "east" *> return T.East

parseWest :: Parser T.Direction
parseWest = string "west" *> return T.West

parseDirection :: Parser T.Direction
parseDirection = do
  skipWhitespaces
  direction <- parseNorth <|> parseSouth <|> parseEast <|> parseWest
  return direction

parseMoveCommand :: Parser T.Command
parseMoveCommand = string "move" *> return T.Move

parseLeftCommand :: Parser T.Command
parseLeftCommand = string "left" *> return T.Left

parseRightCommand :: Parser T.Command
parseRightCommand = string "right" *> return T.Right

parseReportCommand :: Parser T.Command
parseReportCommand = string "report" *> return T.Report

parsePlaceCommand :: Parser T.Command
parsePlaceCommand = do
  _ <- string "place"
  skipSome $ char ' '
  xPos <- integer
  _ <- char ','
  skipMany $ char ' '
  yPos <- integer
  _ <- char ','
  skipMany $ char ' '
  direction <- parseDirection
  return $ Place xPos yPos direction

parseCommand =
  parsePlaceCommand
  <|> parseReportCommand
  <|> parseRightCommand
  <|> parseLeftCommand
  <|> parseMoveCommand

parseCommandString =
  parseString (some (token parseCommand)) mempty
