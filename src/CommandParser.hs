module CommandParser where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta
import Data.Functor

import Type as T

skipWhitespaces :: Parser ()
skipWhitespaces = skipMany (char ' ' <|> char '\n')

parseNorth :: Parser T.Direction
parseNorth = string "north" $> T.North

parseSouth :: Parser T.Direction
parseSouth = string "south" $> T.South

parseEast :: Parser T.Direction
parseEast = string "east" $> T.East

parseWest :: Parser T.Direction
parseWest = string "west" $> T.West

parseDirection :: Parser T.Direction
parseDirection = do
  skipWhitespaces
  parseNorth <|> parseSouth <|> parseEast <|> parseWest

parseMoveCommand :: Parser T.Command
parseMoveCommand = string "move" $> T.Move

parseLeftCommand :: Parser T.Command
parseLeftCommand = string "left" $> T.Left

parseRightCommand :: Parser T.Command
parseRightCommand = string "right" $> T.Right

parseReportCommand :: Parser T.Command
parseReportCommand = string "report" $> T.Report

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
  Place xPos yPos <$> parseDirection

parseCommand =
  parsePlaceCommand
  <|> parseReportCommand
  <|> parseRightCommand
  <|> parseLeftCommand
  <|> parseMoveCommand

parseCommandString =
  parseString (some (token parseCommand)) mempty
