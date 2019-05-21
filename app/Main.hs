{-# LANGUAGE OverloadedStrings #-}
module Main where

import Robot
import Data.Char as C
import System.Environment
import CommandParser as CP
import Game as G
import Control.Monad.Trans.State
import Text.Trifecta

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [fname] -> do
--       content <- readFile fname
--       print content
--       let commands = CP.parseCommandString $ map C.toLower content
--       print commands
--       -- print $ evalState (mapM G.execute commands) G.initGame
--     _ -> putStrLn "Usage: toy-robot filename"

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
                      let (report, newGame) = runState (mapM G.execute c) game
                      case head report of
                        "" -> return newGame
                        _ -> (print $ head report) *> return newGame
                    _ -> putStrLn "Wrong command string" *> return game
      runGame newGame
