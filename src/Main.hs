module Main where

import Control.Monad       (forever)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT, evalStateT, get, put)
import System.IO           (hFlush, stdout)
import System.Random       (randomIO)

import Game (Result(..), Choice, play)
import Util (readLn')

type GameState = (Int, Int, Int)

gameStep :: StateT GameState IO ()
gameStep = do
  lift $ putStr "Rock, Paper, Scissors? " >> hFlush stdout
  player <- lift $ readLn' "Bad spelling, try again."
  computer <- lift (randomIO :: IO Choice)
  lift . putStrLn $ show player ++ " vs " ++ show computer
  result <- lift . return $ play player computer
  lift $ putStrLn ("You " ++ show result ++ "!")
  newScore <- get >>= addScore result
  lift $ displayScore newScore
  put newScore

addScore :: Result -> GameState -> StateT GameState IO GameState
addScore result (won, draw, lost) = case result of
  Won  -> lift $ return (won + 1, draw, lost)
  Draw -> lift $ return (won, draw + 1, lost)
  Lost -> lift $ return (won, draw, lost + 1)

displayScore :: GameState -> IO ()
displayScore (won, draw, lost) =
  putStrLn $ unlines ["+-------+--------+--------+"
                     ,"|  " ++ show Won ++ "  |  " ++ show Draw ++ "  |  " ++ show Lost ++ "  |"
                     ,"+-------+--------+--------+"
                     ,"|   " ++ show won ++ "   |    " ++ show draw ++ "   |    " ++ show lost ++ "   |"
                     ,"+-------+--------+--------+"]

main :: IO ()
main = evalStateT (forever gameStep) (0, 0, 0)
