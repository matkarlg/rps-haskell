module Rps.Scoreboard where

import Rps.GameLogic (Result(..))

type GameState = (Int, Int, Int)

addScore :: Result -> GameState -> GameState
addScore result (won, draw, lost) =
  case result of
    Won  -> (won + 1, draw, lost)
    Draw -> (won, draw + 1, lost)
    Lost -> (won, draw, lost + 1)

displayScore :: GameState -> IO ()
displayScore (won, draw, lost) =
  putStrLn $ unlines ["+-------+--------+--------+"
                     ,"|  " ++ show Won ++ "  |  " ++ show Draw ++ "  |  " ++ show Lost ++ "  |"
                     ,"+-------+--------+--------+"
                     ,"|   " ++ show won ++ "   |    " ++ show draw ++ "   |    " ++ show lost ++ "   |"
                     ,"+-------+--------+--------+"]
