module Main where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (StateT, evalStateT, get, put)
import System.IO           (hFlush, stdout)
import System.Random       (randomIO)

import Rps.GameLogic  (Result(..), Hand, playHands)
import Rps.Scoreboard (GameState, displayScore, addScore)
import Rps.Utils      (readLn')

gameLoop :: StateT GameState IO ()
gameLoop = do
  getScoreBoard <- get

  setScoreBoard <- liftIO $ do
    result <- playRound
    let scoreBoard = addScore result getScoreBoard
    displayScore scoreBoard
    return scoreBoard

  put setScoreBoard

playRound :: IO Result
playRound = do
  putStr "Rock, Paper, Scissors? "
  hFlush stdout
  player <- readLn' "Bad spelling, try again." :: IO Hand
  computer <- randomIO :: IO Hand
  let result = playHands player computer
  putStrLn $ show player ++ " vs " ++ show computer ++ "\n"
  putStrLn $ "You " ++ show result ++ "!"
  return result

main :: IO ()
main = evalStateT (forever gameLoop) (0, 0, 0)
