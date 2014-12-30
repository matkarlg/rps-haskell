{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Applicative ((<$>))
import           Control.Arrow       (first)
import           Control.Monad.Loops (untilM_)
import           Data.Char           (toLower, toUpper)
import           Prelude             hiding (Show (..), print)
import           System.IO           (hFlush, stdout)
import           System.Random       (Random, random, randomIO, randomR)
import           Text.Read           (readMaybe)
import           Text.Show.Text
import           Text.Show.Text.TH   (deriveShow)

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

data Choice = Sten | Sax | Påse
  deriving (Bounded, Enum, Read)
$(deriveShow ''Choice)

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
    Nothing -> T.putStrLn "Felstavning? Försök igen." >> getInput
  where capitalizeWord [] = []
        capitalizeWord (c:cs) = toUpper c : map toLower cs

play :: Choice -> Choice -> Text
play p1 p2 | beats p1 p2 = "Du vann!"
           | beats p2 p1 = "Datorn vann."
           | otherwise   = "Oavgjort."

yesno :: Text -> IO Bool
yesno x = do
  T.putStr x
  hFlush stdout
  input <- T.getLine
  case T.toLower input of
    "y" -> return True
    _   -> return False

gameLoop :: IO ()
gameLoop = do
  T.putStr "Sten, sax eller påse? "
  hFlush stdout
  player   <- getInput
  computer <- randomIO :: IO Choice
  T.putStrLn $ show player `T.append` " VS " `T.append` show computer
  T.putStrLn $ play player computer

main :: IO ()
main = do
  untilM_ gameLoop $ not <$> yesno "Igen? (y/N) "
  putStrLn "Hej då"
