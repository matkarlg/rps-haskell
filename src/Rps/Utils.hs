module Rps.Utils where

import Data.Char (toLower, toUpper)
import Text.Read (readMaybe)

-- | Safe case-insensitive 'readLn' function with error message.
readLn' :: Read a =>
           String  -- ^ Error message
        -> IO a    -- ^ Returns datatype
readLn' e = do
  x <- readMaybe . capitalizeWord <$> getLine
  case x of
    Just z  -> return z
    Nothing -> putStrLn e >> readLn' e
  where capitalizeWord [] = []
        capitalizeWord (c:cs) = toUpper c : map toLower cs
