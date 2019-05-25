{-# LANGUAGE OverloadedStrings #-}
module Main where

import Robot
import Data.Char as C
import System.Environment
import CommandParser as CP
import Game as G
import Control.Monad.Trans.State
import Text.Trifecta
import Data.Functor

main :: IO ()
main = do
  print "- T O Y R O B O T -"
  runGame initGame

runGame :: Game -> IO ()
runGame game = do
  str <- getLine
  case str of
    "exit" -> putStrLn "Exit Game"
    _ -> do
      let command = CP.parseCommandString $ map C.toLower str
      newGame <-  case command of
                    Success c -> do
                      let (report, newGame) = runState (traverse G.execute c) game
                      case head report of
                        "" -> return newGame
                        _ -> (print $ head report) $> newGame
                    _ -> putStrLn "Wrong command string" $> game
      runGame newGame
