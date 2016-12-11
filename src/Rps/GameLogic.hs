module Rps.GameLogic where

import Control.Arrow (first)
import System.Random (Random, random, randomR)

data Result = Won | Draw | Lost
  deriving (Show)

data Hand = Rock | Paper | Scissors
  deriving (Bounded, Enum, Show, Read)

{-# ANN module "HLint: ignore Eta reduce" #-}
instance Random Hand where
-- random :: (Random a, RandomGen g) => g -> (a, g)
  random g = randomR (minBound, maxBound) g

-- randomR :: (Random a, RandomGen g) => (a,a) -> g -> (a, g)
  randomR (a, b) g =
    first toEnum $ randomR (fromEnum a, fromEnum b) g

beats :: Hand -> Hand -> Bool
beats Paper Rock     = True
beats Scissors Paper = True
beats Rock Scissors  = True
beats _ _            = False

playHands :: Hand -> Hand -> Result
playHands p1 p2 | beats p1 p2 = Won
                | beats p2 p1 = Lost
                | otherwise   = Draw
