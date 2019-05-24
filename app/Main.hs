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
                    -- might be nice to represent the existance of a command as
                    -- a `Maybe Command` so the parser combinator concerns are
                    -- isolated in the CommandParser module
                    Success c -> do
                      -- very minor, but mapM is effectively deprecated. `traverse` is the same thing but only requires an Applicative rather than a Monad
                      let (report, newGame) = runState (mapM G.execute c) game
                      -- I think `listToMaybe` is nice for a situation like this
                      case head report of
                        "" -> return newGame
                        -- `pure` is preferable to `return` again because it's on applicative not monad
                        -- hadn't seen *> used much before, this is really nice :)
                        _ -> (print $ head report) *> return newGame
                    _ -> putStrLn "Wrong command string" *> return game
      runGame newGame
