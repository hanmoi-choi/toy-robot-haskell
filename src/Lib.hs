module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Table = Table {
  xSize :: Int
, ySize :: Int
  } deriving (Show, Eq)
