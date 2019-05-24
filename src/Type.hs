{-# LANGUAGE DeriveAnyClass #-}

module Type where

{-
this is really nicely implemented! I like this idea.

I have thought about this a fair bit in regards to having the directouns as
Bounded for my toy robot. I decided I didn't feel comfortable with it because I
don't think it is correct to say a certain direction is the maximum or minimum
direction. Bounded is mostly used for numbers like Int32 which have a clear
maximum and minimum. Having said that it feels like some typeclass like
CyclicEnum should be fine. Perhaps just avoid the Bounded dependency? Dunno ;P
-}
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
  TNone | TLeft | TRight deriving (Show, Eq, Bounded, Enum)

type XPos = Integer
type YPos = Integer
data Command =
  Move | Report | Left | Right | Place XPos YPos Direction
  deriving(Show, Eq)
