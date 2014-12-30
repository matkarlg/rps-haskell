module Main where

import           Control.Applicative ((<$>))
import           Control.Arrow       (first)
import           Control.Monad.Loops (untilM_)
import           Data.Char           (toLower, toUpper)
import           System.IO           (hFlush, stdout)
import           System.Random       (Random, random, randomIO, randomR)
import           Text.Read           (readMaybe)

data Choice = Sten | Sax | Påse
  deriving (Bounded, Enum, Show, Read)

instance Random Choice where
-- random :: (Random a, RandomGen g) => g -> (a, g)
  random g = randomR (minBound, maxBound) g

-- randomR :: (Random a, RandomGen g) => (a,a) -> g -> (a, g)
  randomR (a, b) g =
    first toEnum $ randomR (fromEnum a, fromEnum b) g

beats :: Choice -> Choice -> Bool
beats Påse Sten = True
beats Sax Påse  = True
beats Sten Sax  = True
beats _ _       = False

getInput :: IO Choice
getInput = do
  input <- getLine
  case readMaybe $ capitalizeWord input of
    Just x  -> return x
    Nothing -> putStrLn "Felstavning? Försök igen." >> getInput
  where capitalizeWord [] = []
        capitalizeWord (c:cs) = toUpper c : map toLower cs

play :: Choice -> Choice -> String
play p1 p2 | beats p1 p2 = "Du vann!"
           | beats p2 p1 = "Datorn vann."
           | otherwise   = "Oavgjort."

yesno :: String -> IO Bool
yesno x = do
  putStr x
  hFlush stdout
  input <- getLine
  case map toLower input of
    "y" -> return True
    _   -> return False

gameLoop :: IO ()
gameLoop = do
  putStr "Sten, sax eller påse? "
  hFlush stdout
  player   <- getInput
  computer <- randomIO :: IO Choice
  putStrLn $ show player ++ " VS " ++ show computer
  putStrLn $ play player computer

main :: IO ()
main = do
  untilM_ gameLoop $ not <$> yesno "Igen? (y/N) "
  putStrLn "Hej då"
