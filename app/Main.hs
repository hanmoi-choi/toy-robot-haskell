{-# LANGUAGE OverloadedStrings #-}
module Main where

import Robot
import Data.Char as C
import System.Environment
import CommandParser as CP

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      content <- readFile fname
      print content
      print $ CP.parseCommandString $ map C.toLower content
    _ -> putStrLn "Usage: toy-robot filename"
