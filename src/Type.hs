{-# LANGUAGE DeriveAnyClass #-}

module Type where

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d

    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

data Direction =
    North | East | South | West
    deriving (Eq, Show, Bounded, Enum, CyclicEnum)

data Turn =
    TNone | TLeft | TRight
    deriving (Show, Eq, Bounded, Enum)

type XPos = Integer
type YPos = Integer
data Command =
    Move | Report | Left | Right | Place XPos YPos Direction
    deriving(Show, Eq)
