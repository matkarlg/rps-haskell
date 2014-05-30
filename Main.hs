module Main where

import Control.Arrow (first)
import Control.Monad (when)
import Data.Char     (toLower)
import Data.Maybe    (listToMaybe)
import System.IO     (hFlush, stdout)
import System.Random (Random, random, randomR, randomIO)

data Choice = Sten | Sax | Påse
  deriving (Bounded, Enum, Show)

-- randomIO c/o is derived from the typeclass. (See System.Random source)
-- 1. Send the boundaries of our datatype to randomR :: Choice.
-- 2. Call randomR :: Int with fromEnum.
-- 3. We receive a tuple with the random number and a generator (Int, RandomGen)
-- 4. Convert the random Int back into a Choice.
instance Random Choice where
-- random :: (Random a, RandomGen g) => g -> (a, g)
  random g = randomR (minBound, maxBound) g

-- randomR :: (Random a, RandomGen g) => (a,a) -> g -> (a, g)
  randomR (minB, maxB) g =
    first toEnum $ randomR (fromEnum minB, fromEnum maxB) g

beats :: Choice -> Choice -> Bool
beats Påse Sten = True
beats Sax Påse  = True
beats Sten Sax  = True
beats _ _       = False

getInput :: IO Choice
getInput = input . map toLower =<< getLine

  where input :: String -> IO Choice
        input "sten" = return Sten
        input "sax"  = return Sax
        input "påse" = return Påse
        input _      = putStrLn "Felstavning? Försök igen." >>
                       getInput

play :: Choice -> Choice -> String
play p1 p2 | beats p1 p2 = "Du vann!"
           | beats p2 p1 = "Datorn vann."
           | otherwise   = "Oavgjort."

putStrF :: String -> IO ()
putStrF output = putStr output >> hFlush stdout

yesno :: String -> IO Bool
yesno prompt = do
  putStrF prompt
  input <- fmap listToMaybe getLine
  case fmap toLower input of
    Just 'y' -> return True
    _        -> return False

main :: IO ()
main = do
  putStrF "Sten, sax eller påse? "
  player   <- getInput
  computer <- randomIO :: IO Choice
  putStrLn $ show player ++ " VS " ++ show computer
  putStrLn $ play player computer
  loopMain <- yesno "Igen? (y/N) "
  when loopMain main
