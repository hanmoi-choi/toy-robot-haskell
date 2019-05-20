{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Line where


data Line = Line { _start :: Point, _end :: Point } deriving (Show)
data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeLine :: (Double, Double) -> (Double, Double) -> Line
makeLine start end = Line (makePoint start) (makePoint end)
